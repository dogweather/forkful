---
title:                "Rust: Encontrando o comprimento de uma string"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Porque

Encontrar o comprimento de uma string é uma tarefa comum em muitos programas. Com a linguagem de programação Rust, é possível realizar essa tarefa de forma eficiente e segura, graças ao seu sistema de tipos e gerenciamento de memória. Neste post, vamos explorar como encontrar o comprimento de uma string em Rust.

## Como Fazer

Para encontrar o comprimento de uma string em Rust, podemos usar o método `len()` da struct `String`, que retorna o número de bytes na string. Por exemplo:

```Rust
let minha_string = String::from("Olá, mundo!");
let tamanho = minha_string.len();
println!("A string tem {} bytes.", tamanho);
```

O resultado impresso será: `A string tem 12 bytes.`

Além disso, também é possível encontrar o comprimento de uma string usando o método `len()` da struct `str`, que retorna o número de grafemas (unidades significativas de uma string). Este método é útil para strings que contenham caracteres unicode. Veja um exemplo:

```Rust
let minha_string = "Olá, 🌎!";
let tamanho = minha_string.len();
println!("A string tem {} grafemas.", tamanho);
```

O resultado impresso será: `A string tem 6 grafemas.`

## Deep Dive

Ao encontrar o comprimento de uma string em Rust, é importante considerar que o método `len()` retorna o número de bytes ou grafemas, e não o número de caracteres. Por exemplo, a string `"Olá"` pode ter 3 caracteres, mas tem 5 bytes (2 bytes para cada letra acentuada).

Além disso, é importante mencionar a diferença entre a struct `String` e a struct `str`. Enquanto `String` é uma string de propriedade, ou seja, é capaz de crescer e encolher, `str` é uma string de fatia, usada principalmente para a manipulação de strings imutáveis.

Outra consideração importante é que o método `len()` é limitado ao tamanho de um `usize` (tamanho máximo de um vetor), ou seja, ele não pode retornar um número maior do que o valor máximo definido para `usize` na plataforma em que o Rust está sendo executado.

## Veja Também

- [Documentação Oficial do Rust](https://doc.rust-lang.org/std/string/struct.String.html#method.len)
- [The Rust Programming Language - Chapter 8](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Rust By Example - Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)