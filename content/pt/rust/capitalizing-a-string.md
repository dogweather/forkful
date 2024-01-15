---
title:                "Capitalizando uma string"
html_title:           "Rust: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

Por que alguém gostaria de capitalizar uma string em Rust? Bem, capitalizar uma string é uma tarefa comum ao trabalhar com texto, e é importante saber como fazê-lo corretamente em qualquer linguagem de programação. Neste artigo, vamos explorar como capitalizar uma string usando Rust e também mergulhar um pouco mais fundo para entender melhor o processo.

## Como fazer

Em Rust, existe uma função predefinida para capitalizar uma string: `to_uppercase()`. Você pode usar essa função diretamente em uma variável que contenha uma string ou em uma string literal. Aqui está um exemplo:

```rust
let nome = "joão";
let nome_capitalizado = nome.to_uppercase(); // saída: "JOÃO"
```

Lembre-se de que, em Rust, as variáveis são imutáveis ​​por padrão, portanto, se você quiser alterar o valor da variável original, será necessário atribuir o resultado a uma nova variável ou usar a função `to_uppercase()` diretamente em uma string literal.

Também é possível capitalizar apenas a primeira letra de uma string, em vez de todas as letras. Para fazer isso, podemos usar a função `to_uppercase().next()` da seguinte forma:

```rust
let sobrenome = "silva";
let sobrenome_capitalizado = sobrenome.chars()
                              .next()
                              .map(|primeira| primeira.to_uppercase())
                              .unwrap() + &sobrenome[1..]; // saída: "Silva"
```

## Profundidade

Agora, vamos mergulhar um pouco mais fundo no processo de capitalização de uma string em Rust. Existem algumas coisas importantes a serem observadas, como o uso do UTF-8 em Rust.

Em Rust, as strings são armazenadas como UTF-8 por padrão, o que significa que cada caractere de uma string pode ocupar mais de um byte. Isso pode ser um pouco confuso ao trabalhar com funções de manipulação de strings, como a `to_uppercase()`, que espera que uma string seja composta apenas por caracteres individuais.

Para garantir que nosso código funcione corretamente, é importante garantir que estamos trabalhando com caracteres individuais em vez de bytes. É por isso que, no exemplo acima, usamos a função `chars()` para extrair cada caractere de uma string.

Outra coisa importante a notar é que, ao manipular uma string, estamos essencialmente criando uma nova string com o resultado desejado. Isso ocorre porque, em Rust, as strings são imutáveis ​​por padrão. Portanto, é importante atribuir o resultado de uma operação em uma string a uma nova variável.

## Veja também

- Documentação oficial sobre strings em Rust: <https://doc.rust-lang.org/std/string/index.html>
- Tutorial sobre manipulação de strings em Rust: <https://annamaria.github.io/book/ch02-03-name-input.html>
- Guia abrangente sobre codificação UTF-8 em Rust: <https://github.com/BurntSushi/utf8-ranges>