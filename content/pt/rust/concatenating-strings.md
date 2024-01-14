---
title:    "Rust: Unindo strings"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que Concatenar Strings em Rust?

A concatenação de strings é uma tarefa comum na programação, principalmente quando se trabalha com textos. Em Rust, essa ação pode ser feita de maneira eficiente e simples utilizando suas funcionalidades poderosas. Neste post, vamos explorar por que e como concatenar strings em Rust.

## Como Fazer em Rust

Para concatenar strings em Rust, utilizamos o operador `+` ou a macro `format!()`. Veja os exemplos abaixo:

```
Rust let str1 = "Olá";
let str2 = "mundo";
let resultado = str1 + " " + str2;
println!("{}", resultado);
```
**Saída:** Olá mundo

```
Rust let num1 = 5;
let str1 = format!("O número é {}", num1);
println!("{}", str1);
```
**Saída:** O número é 5

No primeiro exemplo, utilizamos o operador `+` para concatenar as strings `str1` e `str2`, e no segundo exemplo, utilizamos a macro `format!()` para inserir a variável `num1` dentro da string `str1`.

Além disso, em Rust, é possível utilizar a função `push_str()` para adicionar uma string em outra já existente. Veja o exemplo abaixo:

```
Rust let mut cidade = "São";
cidade.push_str(" Paulo");
println!("{}", cidade);
```
**Saída:** São Paulo

## Mergulho Profundo

Assim como a maioria dos recursos em Rust, a concatenação de strings é uma operação segura e eficiente. Isso significa que, quando utilizamos os operadores `+` ou `push_str()`, o compilador irá verificar se há espaço suficiente na memória para a operação e evitar possíveis erros de acesso ou de memória.

Além disso, a macro `format!()` possui uma sintaxe semelhante ao `println!()`, permitindo a formatação de strings de forma mais elaborada e flexível. Também é importante ressaltar que todas essas opções são mais eficientes do que utilizar a função `String::new()` para criar uma nova string e, em seguida, adicionar as strings desejadas utilizando `push_str()`.

## Veja também

Aqui estão alguns links que podem te ajudar a aprofundar seus conhecimentos em Rust:

- Documentação Oficial sobre Strings: https://doc.rust-lang.org/std/string/index.html
- Tutorial de Strings em Rust: https://www.rust-lang.org/learn/strings
- Rust By Example - Manipulação de Strings: https://doc.rust-lang.org/rust-by-example/std/str.html