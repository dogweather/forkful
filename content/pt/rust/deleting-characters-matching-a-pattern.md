---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Rust: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Por que deletar caracteres que correspondem a um padrão em Rust?

Existem várias razões pelas quais alguém pode querer realizar essa tarefa em Rust. Pode ser necessário remover caracteres irrelevantes de uma string antes de realizar alguma operação, ou limpar dados para uma melhor organização e uso posterior.

## Como fazer:

Para deletar caracteres que correspondem a um padrão em Rust, podemos utilizar a função `replace_all()` do módulo `regex`, que nos permite substituir todos os caracteres que correspondem a um padrão por uma string vazia. Veja um exemplo de código abaixo:

```Rust
use regex::Regex;

let string = "Hello, Rust!";
let re = Regex::new("[o,]").unwrap();

let nova_string = re.replace_all(string, "");

println!("{}", nova_string);
```

A saída desse código seria `Hell Rust!`, pois todos os caracteres "o" e "," foram removidos da string original.

Também é possível utilizar a função `replacen()` do módulo `str`, que nos permite substituir um número específico de ocorrências de um padrão por uma string desejada. Veja o exemplo abaixo:

```Rust
let string = "Aprendendo programação em Rust!";
let nova_string = string.replacen("em Rust", "com Rust", 1);

println!("{}", nova_string);
```

A saída desse código seria `Aprendendo programação com Rust!`, pois apenas a primeira ocorrência da string "em Rust" foi substituída.

## Mergulho Profundo:

Ao manipular strings em Rust, é importante ter em mente que elas são imutáveis por padrão. Isso significa que, ao realizar uma operação como a substituição de caracteres, uma nova string será criada, deixando a original intacta. Isso pode ser um pouco ineficiente em termos de desempenho, principalmente para strings grandes.

No entanto, a linguagem possui o conceito de *ownership*, onde a referência de um dado pode ser transferida de uma variável para outra. Portanto, podemos usar esse conceito para evitar a criação de uma nova string ao realizar a substituição de caracteres. Veja um exemplo abaixo:

```Rust
let mut string = String::from("Hello, Rust!");

let nova_string = string.replace("o", "");

println!("{}", nova_string);
```

Neste exemplo, ao invés de criar uma nova string, estamos modificando diretamente a string original. Isso pode trazer um melhor desempenho em casos específicos, mas é importante tomar cuidado pois a referência da string original será perdida.

# Veja também:

- [Documentação do módulo `regex` em Rust](https://docs.rs/regex/1.5.4/regex/)
- [Documentação do módulo `str` em Rust](https://doc.rust-lang.org/std/primitive.str.html)
- [Tutorial sobre string manipulation em Rust](https://www.learn-rust.org/2020/07/13/string-manipulation-in-rust.html)