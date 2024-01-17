---
title:                "Interpolando uma string."
html_title:           "Rust: Interpolando uma string."
simple_title:         "Interpolando uma string."
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Ao programar em Rust, você provavelmente já se deparou com a necessidade de juntar uma string com valores de variáveis. É aí que entra o conceito de interpolação de strings - uma forma de combinar uma string com outras variáveis ou valores.
Interpolação de strings é uma técnica comum usada por programadores para criar strings dinâmicas, que mudam de acordo com os valores de variáveis. É uma forma mais eficiente e elegante de criar strings do que a concatenação manual de caracteres.

## Como fazer:

Em Rust, a interpolação de strings é feita utilizando a macro ```format!()``` . Confira este exemplo:

``` Rust
let name = "João";
let age = 28;

let message = format!("Olá, meu nome é {}, tenho {} anos", name, age);
println!("{}", message);
```

A saída deste código será: ```Olá, meu nome é João, tenho 28 anos```.

Outro exemplo mostrando como utilizar a interpolação de strings dentro de uma função:

``` Rust
fn welcome_message(name: &str, age: u32) {
    println!("Bem-vindo, {}, você tem {} anos.", name, age);
}

let name = "Maria";
let age = 32;

welcome_message(name, age);
```
A saída deste código será: ```Bem-vindo, Maria, você tem 32 anos.```

## Aprofundando:

A interpolação de strings é uma técnica comum utilizada por diversas linguagens de programação, como Java, Python e C++. Em Rust, essa funcionalidade é provida pela macro ```format!()```. Esta macro suporta diversas funcionalidades, tais como formatação de números, data e hora, e alinhamento de texto.

Outra forma de interpolação de strings em Rust é utilizando a sintaxe de placeholders ```{}```, onde a string é composta por placeholders que serão substituídos pelos valores das variáveis. Veja um exemplo:

``` Rust
let first_name = "Ana";
let last_name = "Silva";

println!("{} {}", first_name, last_name);
```

A saída deste código será: ```Ana Silva```.

## Veja também:

- [Documentação oficial do Rust sobre a macro format!()](https://doc.rust-lang.org/std/fmt/macro.format.html)
- [Exemplos de uso da macro format!()](https://cheats.rs/#std::fmt::format!)
- [Tutorial sobre interpolação de strings em Rust](https://blog.logrocket.com/string-interpolation-in-rust-a-quick-tutorial/)