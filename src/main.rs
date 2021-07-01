use std::io::{self, Write};
mod lexer;
mod token;
mod parser;
mod ast;
mod object;
mod evaluator;

use lexer::*;
use parser::*;
use ast::*;
use evaluator::*;

fn main() -> io::Result<()> {
    let prompt = ">>";
    println!("Hello mrnugget! This is the Monkey programming language!\nFeel free to type in commands");
    
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

        let evaluated = eval(Node::Prog(program));
        if let Ok(obj) = evaluated {
            println!("{}", obj.inspect());
        }
    }
}

fn print_parser_errors(errors: &Vec<String>) {
    for e in errors.iter() {
        println!("\t{}", e);
    }
}
