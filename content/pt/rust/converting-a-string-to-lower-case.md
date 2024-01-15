---
title:                "Convertendo uma string para minúsculas."
html_title:           "Rust: Convertendo uma string para minúsculas."
simple_title:         "Convertendo uma string para minúsculas."
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Você já precisou converter uma string para letras minúsculas em seus projetos de programação? É uma tarefa comum e, com a linguagem Rust, é uma tarefa simples de realizar. Neste artigo, vamos explorar o porquê de você querer fazer essa conversão e o melhor caminho para alcançá-la.

## Como Fazer

Você pode usar o método `to_lowercase()` para converter uma string para letras minúsculas em Rust. Aqui está um exemplo de código:

```Rust
let string = "Olá, Mundo!";
let result = string.to_lowercase();

println!("{}", result);
```

O resultado da execução deste código será `olá, mundo!` como esperado. Podemos até mesmo aplicar esse método diretamente em uma string literal, veja:

```Rust
let result = "Aqui Está uma STRING".to_lowercase();
```

O resultado do código acima será `aqui está uma string`.

Há também a opção de usar a função `chars()` em conjunto com o método `to_lowercase()` para converter cada caractere individual da string para minúsculo. Aqui está um exemplo que mostra a saída para cada caractere individual:

```Rust
let string = "RUST";
let lowercased: String = string.chars()
    .map(|c| c.to_lowercase())
    .collect();

println!("{}", lowercased);
```

O resultado da execução deste código será `rust`, mostrando como cada caractere foi convertido para minúsculo individualmente.

## Fundo

Ao trabalhar com strings em Rust, é importante entender como elas são representadas no código. Strings são uma coleção de caracteres do tipo `char`, que é um tipo de dado que pode representar qualquer caractere Unicode. Isso significa que, ao converter uma string para letras minúsculas, a linguagem precisa estar ciente do conjunto completo de caracteres Unicode. Felizmente, Rust possui uma vasta biblioteca de suporte para lidar com esse tipo de conversão.

Quando usamos o método `to_lowercase()`, estamos na verdade usando a função `to_lowercase()` da biblioteca padrão, que faz parte do módulo `std::string`. Esta função é definida como `fn to_lowercase(self: &str) -> String`, o que significa que ela retorna uma nova string após fazer a conversão para minúsculo.

## Veja Também

- [Documentação da função `to_lowercase()` na biblioteca padrão do Rust](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- [Guia completo sobre manipulação de strings em Rust](https://crates.io/crates/strum)
- [Exemplos de uso do método `to_lowercase()`](https://github.com/rust-lang/rust/blob/master/src/libcollections/str.rs#L94-L115)