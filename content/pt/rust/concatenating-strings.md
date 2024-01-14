---
title:                "Rust: Unindo strings"
simple_title:         "Unindo strings"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

##Porque Concatenar Strings em Rust é Importante

Concatenar strings é uma tarefa comum em programação e pode ser especialmente útil em Rust. Ao concatenar strings, é possível unir várias strings em uma única string, permitindo que sejam manipuladas e apresentadas de forma mais eficiente.

##Como Concatenar Strings em Rust

É simples concatenar strings em Rust usando o operador `+`. Veja o exemplo abaixo:

```Rust 
let primeiro_nome = "João";
let sobrenome = "da Silva";

let nome_completo = primeiro_nome + " " + sobrenome;

println!("{}, bem-vindo!", nome_completo);
```

Esse código irá produzir a seguinte saída:

```
João da Silva, bem-vindo!
```

Também é possível concatenar mais de duas strings de uma vez:

```Rust
let primeira_parte = "Eu";
let segunda_parte = "amo";
let terceira_parte = "Rust";

let frase = primeira_parte + " " + segunda_parte + " " + terceira_parte;

println!("{}", frase);
```

A saída será:

```
Eu amo Rust
```

##Aprofundando em Concatenação de Strings em Rust

Ao concatenar strings em Rust, é importante observar que o tipo de dado resultante é sempre `String`. Isso significa que é possível armazenar o resultado da concatenação em uma variável e continuar manipulando a string normalmente.

Outra coisa importante a se notar é que o operador `+` não funciona apenas com o tipo `&str` (referência para sequência de caracteres). Ele também funciona com o tipo `String`, o que torna o processo de concatenação ainda mais flexível.

Outra opção para concatenar strings em Rust é através do método `format!()`. Esse método cria uma nova string a partir de strings existentes, como no exemplo abaixo:

```Rust
let primeiro_nome = "Maria";
let sobrenome = "dos Santos";

let nome_completo = format!("{} {}", primeiro_nome, sobrenome);

println!("{}, seja bem-vinda!", nome_completo);
```

A saída será:

```
Maria dos Santos, seja bem-vinda!
```

##Veja também

- [Documentação oficial de Strings em Rust](https://doc.rust-lang.org/std/string/index.html)
- [Concatenação de Strings em Rust - Tutorialspoint](https://www.tutorialspoint.com/rust/rust_string.htm)
- [Deep Dive: Entendendo Referências em Rust](https://www.rust-lang.org/pt-BR/learn/official-documents/book/second-edition/ch04-00-understanding-ownership)