---
title:                "Combinando strings"
html_title:           "Rust: Combinando strings"
simple_title:         "Combinando strings"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Se você está trabalhando com algum tipo de linguagem que envolve a concatenação de strings, provavelmente já se deparou com alguns desafios. Afinal, juntar diferentes trechos de texto pode ser uma tarefa complicada e até mesmo tediosa dependendo da linguagem que você está utilizando. Felizmente, Rust tem um sistema de tipos forte que facilita bastante essa tarefa.

## Como Fazer

A concatenação de strings em Rust é feita por meio do operador `+` ou do método `format!`. Vamos dar uma olhada em ambos os métodos:

```
let first_name = "Maria";
let last_name = "Silva";

let full_name = first_name + " " + last_name;
println!("Seu nome completo é: {}", full_name); // Saída: Seu nome completo é: Maria Silva

// Ou utilizando o método format!
let full_name = format!("{} {}", first_name, last_name);
println!("Seu nome completo é: {}", full_name); // Saída: Seu nome completo é: Maria Silva
```

Como você pode ver, ambos os métodos produzem o mesmo resultado. No entanto, o método `format!` permite que você utilize placeholders (`{}`) para especificar onde você quer inserir os valores. Além disso, com o método `format!` você pode juntar mais do que apenas duas strings. Veja este exemplo:

```
let first_name = "João";
let last_name = "Santos";
let city = "São Paulo";
let country = "Brasil";

let full_info = format!("Nome: {}, Sobrenome: {}, Cidade: {}, País: {}", first_name, last_name, city, country);
println!("Informações completas: {}", full_info); // Saída: Informações completas: Nome: João, Sobrenome: Santos, Cidade: São Paulo, País: Brasil
```

## Mergulho Profundo

É importante notar que a concatenação de strings pode ser uma operação custosa em termos de performance, especialmente se você está fazendo isso repetidamente em um loop, por exemplo. Isso acontece porque cada vez que você concatena uma string, uma nova cópia é criada, o que pode resultar em muita alocação de memória. Para evitar isso, Rust tem algumas opções otimizadas, como o tipo `String` e o tipo `StringBuilder`, que permitem que você construa strings de forma mais eficiente.

Além disso, é importante lembrar que strings em Rust são imutáveis. Isso significa que todas as operações de concatenação resultam em uma nova string e não modificam a string original. Se você está trabalhando com strings que precisam ser modificadas, é importante considerar o uso de outros tipos, como `Vec<u8>`.

## Veja Também

- [Documentação oficial de strings em Rust](https://doc.rust-lang.org/std/string/)
- [Tutoriais e exemplos práticos de strings em Rust](https://www.rust-lang.org/learn/strings)