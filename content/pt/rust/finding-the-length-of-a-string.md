---
title:                "Encontrando o comprimento de uma cadeia de caracteres"
html_title:           "Rust: Encontrando o comprimento de uma cadeia de caracteres"
simple_title:         "Encontrando o comprimento de uma cadeia de caracteres"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Encontrar o tamanho de uma string em programação significa descobrir quantos caracteres existem naquela string. Isso é útil porque permite que os programadores manipulem e usem essas strings de maneira eficiente em seus códigos.

## Como fazer:

Existem algumas maneiras de encontrar o tamanho de uma string em Rust. Uma maneira é usando o método `len ()`, que retorna o número de bytes na string. Veja o exemplo abaixo:

```Rust
let string = String::from("Olá mundo!");
println!("Tamanho da string: {}", string.len());
```

A saída será:

```
Tamanho da string: 11
```

Outra maneira é usar o método `chars ()`, que retorna um iterador de cada caractere na string. Então, você pode usar o método `count ()` para contar o número de caracteres na string. Veja o exemplo abaixo:

```Rust
let string = String::from("Olá mundo!");
println!("Tamanho da string: {}", string.chars().count());
```

A saída será a mesma: 11.

## Mergulho profundo:

Historicamente, encontrar o tamanho de uma string tem sido um desafio para os programadores, pois muitas linguagens exigiam que o desenvolvedor aloque espaço suficiente na memória antes de adicionar uma string a um array de caracteres. Em Rust, isso é tratado automaticamente pelo tipo String, que armazena o tamanho da string junto com o seu conteúdo.

Existem algumas alternativas para encontrar o tamanho de uma string em Rust, sendo a mais comum o uso do método `len ()` mencionado anteriormente.

## Veja também:

- Documentação oficial do método `len ()`: https://doc.rust-lang.org/std/string/struct.String.html#method.len
- Outras maneiras de trabalhar com strings em Rust: https://www.geeksforgeeks.org/working-strings-rust/