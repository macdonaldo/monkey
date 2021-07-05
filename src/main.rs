use std::io::{self, Write};
mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod token;

use ast::*;
use evaluator::*;
use lexer::*;
use object::*;
use parser::*;

fn main() -> io::Result<()> {
    let mut env = Environment::new();
    let prompt = ">>";
    println!(
        "Hello mrnugget! This is the Monkey programming language!\nFeel free to type in commands"
    );

    loop {
        print!("{} ", prompt);
        let mut input = String::new();
        let _ = io::stdout().flush(); // needed since prompt does not contain a new line char
        io::stdin().read_line(&mut input)?;

        if input.is_empty() {
            return Ok(());
        }

        let l = Lexer::new(input.chars().collect());
        let mut p = Parser::new(l);
        let program = p.parse_program();

        const MONKEY_ART: &str = r#"
        ┈┈┈┈┈┈┈┈┈┈┈?????????????
        ┈┈╱▔▔▔▔▔╲┈┈┈??????????
        ┈╱┈┈╱▔╲╲╲▏┈┈┈?????┈
        ╱┈┈╱━╱▔▔▔▔▔╲━╮┈┈
        ▏┈▕┃▕╱▔╲╱▔╲▕╮┃┈┈
        ▏┈▕╰━▏▊▕▕▋▕▕━╯┈┈
        ╲┈┈╲╱▔╭╮▔▔┳╲╲┈┈┈
        ┈╲┈┈▏╭━━━━╯▕▕┈┈┈
        ┈┈╲┈╲▂▂▂▂▂▂╱╱┈┈┈
        ┈┈┈┈▏┊┈┈┈┈┊┈┈┈╲┈
        ┈┈┈┈▏┊┈┈┈┈┊▕╲┈┈╲
        ┈╱▔╲▏┊┈┈┈┈┊▕╱▔╲▕
        ┈▏┈┈┈╰┈┈┈┈╯┈┈┈▕▕
        ┈╲┈┈┈╲┈┈┈┈╱┈┈┈╱┈╲
        ┈┈╲┈┈▕▔▔▔▔▏┈┈╱╲╲╲▏
        ┈╱▔┈┈▕┈┈┈┈▏┈┈▔╲▔▔
        ┈╲▂▂▂╱┈┈┈┈╲▂▂▂╱┈
        "#;

        if !program.errors.is_empty() {
            println!("{}", MONKEY_ART);
            println!("Woops! We ran into some monkey business here!");
            println!(" parser errors:");
            print_parser_errors(&program.errors);
            continue;
        }
        let evaluated = eval(Node::Prog(program), &mut env);
        if let Ok(obj) = evaluated {
            match obj {
                Object::Null => (),
                _ => println!("{}", obj.inspect()),
            }
        }
    }
}

fn print_parser_errors(errors: &Vec<String>) {
    for e in errors.iter() {
        println!("\t{}", e);
    }
}
