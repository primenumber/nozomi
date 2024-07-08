use std::io::Read;
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

#[derive(Clone, Debug)]
enum Inst {
    Add(u8),
    MovePtr(isize),
    Init(u8),
    GetChar,
    PutChar,
    Loop(Rc<Vec<Inst>>),
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
                loop_stack.last_mut()?.push(Inst::Loop(Rc::new(l)));
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
                    *y += x;
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
                result.push(Inst::Loop(Rc::new(optimize_basic(l))));
            }
            _ => {
                result.push(inst.clone());
            }
        }
    }
    result
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

fn exec_body(insts: &[Inst], memory: &mut Vec<u8>, ptr: &mut usize) -> Result<(), std::io::Error> {
    for inst in insts {
        match inst {
            Inst::Add(x) => memory[*ptr] = memory[*ptr].wrapping_add(*x),
            Inst::MovePtr(x) => {
                *ptr = ptr.wrapping_add_signed(*x);
                if *ptr >= memory.len() {
                    memory.resize(*ptr + 1, 0);
                }
            }
            Inst::Init(x) => memory[*ptr] = *x,
            Inst::GetChar => memory[*ptr] = getchar()?,
            Inst::PutChar => putchar(memory[*ptr])?,
            Inst::Loop(l) => {
                while memory[*ptr] != 0 {
                    exec_body(l, memory, ptr)?;
                }
            }
        }
    }
    Ok(())
}

fn exec(insts: &[Inst]) -> Result<(), std::io::Error> {
    let mut ptr = 0;
    let mut memory = vec![0u8; 30000];
    exec_body(insts, &mut memory, &mut ptr)
}

fn main() -> Result<(), std::io::Error> {
    let args: Vec<_> = std::env::args().collect();
    let code = std::fs::read_to_string(&args[1])?;
    let raw_insts = parse(&code);
    let insts = extract_loops(&raw_insts).unwrap();
    let insts = optimize_basic(&insts);
    exec(&insts)
}
