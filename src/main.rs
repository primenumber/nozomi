use std::cmp::max;
use std::io::Read;
use std::ops::Index;
use std::ops::IndexMut;

enum RawInst {
    AddI(u8),
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
            '+' => insts.push(RawInst::AddI(1)),
            '-' => insts.push(RawInst::AddI(255)),
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
    AddI(u8),
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
            RawInst::AddI(x) => last.push(Inst::AddI(*x)),
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
            Inst::AddI(x) => {
                if let Some(Inst::AddI(y)) = result.last_mut() {
                    *y = y.wrapping_add(*x);
                } else if let Some(Inst::Init(y)) = result.last_mut() {
                    *y = y.wrapping_add(*x);
                } else {
                    result.push(Inst::AddI(*x));
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
                if optimized_body == vec![Inst::AddI(255)] {
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
    AddI(isize, u8),
    MovePtr(isize),
    Init(isize, u8),
    AddMul(isize, isize, u8),
    GetChar(isize),
    PutChar(isize),
    Loop(Vec<InstWithOffset>),
}

fn annotate_offset(insts: &[Inst]) -> Vec<InstWithOffset> {
    insts
        .iter()
        .map(|inst| match inst {
            Inst::AddI(x) => InstWithOffset::AddI(0, *x),
            Inst::MovePtr(x) => InstWithOffset::MovePtr(*x),
            Inst::Init(x) => InstWithOffset::Init(0, *x),
            Inst::GetChar => InstWithOffset::GetChar(0),
            Inst::PutChar => InstWithOffset::PutChar(0),
            Inst::Loop(l) => InstWithOffset::Loop(annotate_offset(l)),
        })
        .collect()
}

fn delay_move_ptr(insts: &[InstWithOffset]) -> Vec<InstWithOffset> {
    let mut offset = 0;
    let mut result = Vec::new();
    for inst in insts {
        match inst {
            InstWithOffset::AddI(ofs, x) => result.push(InstWithOffset::AddI(ofs + offset, *x)),
            InstWithOffset::MovePtr(x) => offset += *x,
            InstWithOffset::Init(ofs, x) => result.push(InstWithOffset::Init(ofs + offset, *x)),
            InstWithOffset::AddMul(ofs1, ofs2, x) => {
                result.push(InstWithOffset::AddMul(*ofs1 + offset, *ofs2 + offset, *x))
            }
            InstWithOffset::GetChar(ofs) => result.push(InstWithOffset::GetChar(ofs + offset)),
            InstWithOffset::PutChar(ofs) => result.push(InstWithOffset::PutChar(ofs + offset)),
            InstWithOffset::Loop(l) => {
                result.push(InstWithOffset::MovePtr(offset));
                offset = 0;
                let new_l = delay_move_ptr(l);
                result.push(InstWithOffset::Loop(new_l));
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
            InstWithOffset::Loop(l) => Some(InstWithOffset::Loop(remove_zero_move_ptr(l))),
            _ => Some(inst.clone()),
        })
        .collect()
}

fn loop_to_addmul_body(insts: &[InstWithOffset]) -> Option<Vec<InstWithOffset>> {
    let mut base_add = 0u8;
    let mut add_ops = Vec::new();
    for inst in insts {
        if let InstWithOffset::AddI(ofs, x) = inst {
            if *ofs == 0 {
                base_add = base_add.wrapping_add(*x);
            } else {
                add_ops.push((*ofs, *x));
            }
        } else {
            return None;
        }
    }
    if base_add != 255 {
        return None;
    }
    let mut result = Vec::new();
    for (ofs, x) in add_ops {
        result.push(InstWithOffset::AddMul(0, ofs, x));
    }
    result.push(InstWithOffset::Init(0, 0));
    Some(result)
}

fn loop_to_addmul(insts: &[InstWithOffset]) -> Vec<InstWithOffset> {
    insts
        .iter()
        .map(|inst| match inst {
            InstWithOffset::Loop(l) => {
                if let Some(new_insts) = loop_to_addmul_body(l) {
                    //eprintln!("{:?} => {:?}", l, new_insts);
                    InstWithOffset::Loop(new_insts)
                } else {
                    InstWithOffset::Loop(loop_to_addmul(l))
                }
            }
            _ => inst.clone(),
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
            InstWithOffset::AddI(ofs, x) => {
                let index = ptr.checked_add_signed(*ofs).unwrap();
                memory[index] = memory[index].wrapping_add(*x)
            }
            InstWithOffset::MovePtr(x) => {
                *ptr = ptr.checked_add_signed(*x).unwrap();
            }
            InstWithOffset::Init(ofs, x) => {
                let index = ptr.wrapping_add_signed(*ofs);
                memory[index] = *x;
            }
            InstWithOffset::AddMul(ofs1, ofs2, x) => {
                let index1 = ptr.checked_add_signed(*ofs1).unwrap();
                let index2 = ptr.checked_add_signed(*ofs2).unwrap();
                memory[index2] = memory[index2].wrapping_add(memory[index1].wrapping_mul(*x))
            }
            InstWithOffset::GetChar(ofs) => {
                let index = ptr.checked_add_signed(*ofs).unwrap();
                memory[index] = getchar()?
            }
            InstWithOffset::PutChar(ofs) => {
                let index = ptr.checked_add_signed(*ofs).unwrap();
                putchar(memory[index])?
            }
            InstWithOffset::Loop(l) => {
                *cycle_count += 1;
                while memory[*ptr] != 0 {
                    exec_body(l, memory, ptr, cycle_count)?;
                    *cycle_count += 1;
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
    let insts = loop_to_addmul(&insts);
    exec(&insts)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_loop_to_addmul() {
        let insts = [
            InstWithOffset::AddI(0, 2),
            InstWithOffset::MovePtr(2),
            InstWithOffset::Loop(vec![
                InstWithOffset::AddI(0, 255),
                InstWithOffset::AddI(1, 1),
            ]),
        ];
        assert_eq!(
            loop_to_addmul(&insts),
            vec![
                InstWithOffset::AddI(0, 2),
                InstWithOffset::MovePtr(2),
                InstWithOffset::AddMul(0, 1, 1),
                InstWithOffset::Init(0, 0)
            ]
        );
    }
}
