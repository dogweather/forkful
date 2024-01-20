---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?

Concatenar strings é a operação de juntar duas ou mais strings numa só. Programadores fazem isso para manipular e combinar textos de maneira eficiente.

## Como fazer:

Em Rust, você pode concatenar strings de várias maneiras:

1. Usando o operador '+':

```Rust
let s1 = "Olá".to_string();
let s2 = ", mundo";
let s3 = s1 + s2;
println!("{}", s3);
```
Resultado: `Olá, mundo`

2. Usando o método `format!`:

```Rust
let s1 = "Olá";
let s2 = ", mundo";
let s3 = format!("{}{}", s1, s2);
println!("{}", s3);
```
Resultado: `Olá, mundo`

## Mergulho Profundo:

Concatenar strings é uma prática fundamental em programação desde seus primórdios. No Rust, a concatenação de strings foi projetada para ser eficiente, tanto em termos de velocidade como de memória.

Existem alternativas à concatenação de strings em programação. A mais notável é o uso de intercalação de strings, que também é suportada pelo Rust através de marcas especiais em strings.

É importante notar que a concatenação de strings no Rust assume a posse depois que uma operação é feita. Isso significa que, depois de concatenar duas strings, a string original não pode ser usada novamente. Isso é diferente de outras linguagens de programação, onde a string original permanece inalterada após a concatenação.

## Veja Também:

1. Documentação oficial Rust [std::string::String](https://doc.rust-lang.org/std/string/struct.String.html)
2. [Concatenate strings in Rust](https://stackoverflow.com/questions/30154541/how-do-i-concatenate-strings)