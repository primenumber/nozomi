use std::cmp::max;
use std::io::Read;
use std::ops::Index;
use std::ops::IndexMut;
use std::rc::Rc;

enum RawInst {
    Add(u8),
    MovePtr(isize),
    GetChar,
    PutChar,
    StartLoop,
    EndLoop,
}

fn parse(code: &str) -> Vec<RawInst> {
    let mut insts = Vec::new();
    for ch in code.chars() {
        match ch {
            '+' => insts.push(RawInst::Add(1)),
            '-' => insts.push(RawInst::Add(255)),
            '>' => insts.push(RawInst::MovePtr(1)),
            '<' => insts.push(RawInst::MovePtr(-1)),
            '[' => insts.push(RawInst::StartLoop),
            ']' => insts.push(RawInst::EndLoop),
            '.' => insts.push(RawInst::PutChar),
            ',' => insts.push(RawInst::GetChar),
            _ => (),
        }
    }
    insts
}

#[derive(Clone, Debug, PartialEq)]
enum Inst {
    Add(u8),
    MovePtr(isize),
    Init(u8),
    GetChar,
    PutChar,
    Loop(Vec<Inst>),
}

fn extract_loops(raw_insts: &[RawInst]) -> Option<Vec<Inst>> {
    let mut loop_stack = Vec::new();
    loop_stack.push(Vec::new());
    for raw_inst in raw_insts {
        let last = loop_stack.last_mut()?;
        match raw_inst {
            RawInst::Add(x) => last.push(Inst::Add(*x)),
            RawInst::MovePtr(x) => last.push(Inst::MovePtr(*x)),
            RawInst::GetChar => last.push(Inst::GetChar),
            RawInst::PutChar => last.push(Inst::PutChar),
            RawInst::StartLoop => loop_stack.push(Vec::new()),
            RawInst::EndLoop => {
                let l = loop_stack.pop()?;
                loop_stack.last_mut()?.push(Inst::Loop(l));
            }
        }
    }
    if loop_stack.len() != 1 {
        return None;
    }
    loop_stack.pop()
}

fn optimize_basic(insts: &[Inst]) -> Vec<Inst> {
    let mut result = Vec::new();
    for inst in insts {
        match inst {
            Inst::Add(x) => {
                if let Some(Inst::Add(y)) = result.last_mut() {
                    *y = y.wrapping_add(*x);
                } else if let Some(Inst::Init(y)) = result.last_mut() {
                    *y = y.wrapping_add(*x);
                } else {
                    result.push(Inst::Add(*x));
                }
            }
            Inst::MovePtr(x) => {
                if let Some(Inst::MovePtr(y)) = result.last_mut() {
                    *y += x;
                } else {
                    result.push(Inst::MovePtr(*x));
                }
            }
            Inst::Loop(l) => {
                let optimized_body = optimize_basic(l);
                if optimized_body == vec![Inst::Add(255)] {
                    result.push(Inst::Init(0));
                } else {
                    result.push(Inst::Loop(optimized_body));
                }
            }
            _ => {
                result.push(inst.clone());
            }
        }
    }
    result
}

#[derive(Clone, Debug, PartialEq)]
enum InstWithOffset {
    Add(isize, u8),
    MovePtr(isize),
    Init(isize, u8),
    //Dup(isize, isize),
    GetChar(isize),
    PutChar(isize),
    Loop(isize, Vec<InstWithOffset>),
}

fn annotate_offset(insts: &[Inst]) -> Vec<InstWithOffset> {
    insts
        .iter()
        .map(|inst| match inst {
            Inst::Add(x) => InstWithOffset::Add(0, *x),
            Inst::MovePtr(x) => InstWithOffset::MovePtr(*x),
            Inst::Init(x) => InstWithOffset::Init(0, *x),
            Inst::GetChar => InstWithOffset::GetChar(0),
            Inst::PutChar => InstWithOffset::PutChar(0),
            Inst::Loop(l) => InstWithOffset::Loop(0, annotate_offset(l)),
        })
        .collect()
}

fn delay_move_ptr(insts: &[InstWithOffset]) -> Vec<InstWithOffset> {
    let mut offset = 0;
    let mut result = Vec::new();
    for inst in insts {
        match inst {
            InstWithOffset::Add(ofs, x) => result.push(InstWithOffset::Add(ofs + offset, *x)),
            InstWithOffset::MovePtr(x) => offset += *x,
            InstWithOffset::Init(ofs, x) => result.push(InstWithOffset::Init(ofs + offset, *x)),
            //InstWithOffset::Dup(ofs1, ofs2) => {
            //    result.push(InstWithOffset::Dup(*ofs1 + offset, *ofs2 + offset))
            //}
            InstWithOffset::GetChar(ofs) => result.push(InstWithOffset::GetChar(ofs + offset)),
            InstWithOffset::PutChar(ofs) => result.push(InstWithOffset::PutChar(ofs + offset)),
            InstWithOffset::Loop(_ofs, l) => {
                result.push(InstWithOffset::MovePtr(offset));
                offset = 0;
                let new_l = delay_move_ptr(l);
                result.push(InstWithOffset::Loop(0, new_l));
            }
        }
    }
    if offset != 0 {
        result.push(InstWithOffset::MovePtr(offset));
    }
    result
}

fn remove_zero_move_ptr(insts: &[InstWithOffset]) -> Vec<InstWithOffset> {
    insts
        .iter()
        .filter_map(|inst| match inst {
            InstWithOffset::MovePtr(x) => {
                if *x != 0 {
                    Some(inst.clone())
                } else {
                    None
                }
            }
            InstWithOffset::Loop(ofs, l) => {
                Some(InstWithOffset::Loop(*ofs, remove_zero_move_ptr(l)))
            }
            _ => Some(inst.clone()),
        })
        .collect()
}

fn getchar() -> Result<u8, std::io::Error> {
    let mut buf = [0u8];
    std::io::stdin().lock().read_exact(&mut buf)?;
    Ok(buf[0])
}

fn putchar(x: u8) -> Result<(), std::io::Error> {
    print!("{}", x as char);
    Ok(())
}

struct Memory(Vec<u8>);

impl Index<usize> for Memory {
    type Output = u8;
    fn index(&self, index: usize) -> &Self::Output {
        self.0.get(index).unwrap_or(&0)
    }
}

impl IndexMut<usize> for Memory {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        let current_len = self.0.len();
        if index >= current_len {
            let next_len = max(index + 1, max(current_len * 2, 30000));
            self.0.resize(next_len, 0);
        }
        &mut self.0[index]
    }
}

fn exec_body(
    insts: &[InstWithOffset],
    memory: &mut Memory,
    ptr: &mut usize,
    cycle_count: &mut usize,
) -> Result<(), std::io::Error> {
    for inst in insts {
        *cycle_count += 1;
        match inst {
            InstWithOffset::Add(ofs, x) => {
                let index = ptr.wrapping_add_signed(*ofs);
                memory[index] = memory[index].wrapping_add(*x)
            }
            InstWithOffset::MovePtr(x) => {
                *ptr = ptr.wrapping_add_signed(*x);
            }
            InstWithOffset::Init(ofs, x) => {
                let index = ptr.wrapping_add_signed(*ofs);
                memory[index] = *x;
            }
            InstWithOffset::GetChar(ofs) => {
                let index = ptr.wrapping_add_signed(*ofs);
                memory[index] = getchar()?
            }
            InstWithOffset::PutChar(ofs) => {
                let index = ptr.wrapping_add_signed(*ofs);
                putchar(memory[index])?
            }
            InstWithOffset::Loop(ofs, l) => {
                while memory[ptr.wrapping_add_signed(*ofs)] != 0 {
                    exec_body(l, memory, ptr, cycle_count)?;
                }
            }
        }
    }
    Ok(())
}

fn exec(insts: &[InstWithOffset]) -> Result<(), std::io::Error> {
    let mut ptr = 0;
    let mut memory = Memory(vec![0u8; 30000]);
    let mut cycle_count = 0;
    exec_body(insts, &mut memory, &mut ptr, &mut cycle_count)?;
    eprintln!("cycles: {}", cycle_count);
    Ok(())
}

fn main() -> Result<(), std::io::Error> {
    let args: Vec<_> = std::env::args().collect();
    let code = std::fs::read_to_string(&args[1])?;
    let raw_insts = parse(&code);
    let insts = extract_loops(&raw_insts).unwrap();
    let insts = optimize_basic(&insts);
    let insts = annotate_offset(&insts);
    let insts = delay_move_ptr(&insts);
    let insts = remove_zero_move_ptr(&insts);
    exec(&insts)
}
