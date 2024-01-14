---
title:    "Rust: Buscando e substituindo texto"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Alguma vez você já precisou fazer alterações em um grande arquivo de texto e ficou exausto de procurar e substituir manualmente? Bem, a linguagem de programação Rust tem uma solução para isso! Através do seu sistema de gerenciamento de memória e poderosas ferramentas de manipulação de texto, Rust torna a busca e substituição de texto rápida e eficiente.

## Como Fazer

Para realizar busca e substituição de texto em Rust, podemos usar a função "replace" da biblioteca padrão. Primeiro, vamos definir uma string para realizar a busca:

```Rust
let texto = "Olá, mundo! Meu nome é Rust.";
```

Agora, vamos usar o método "replace" para substituir a palavra "Rust" por "Programação":

```Rust
let novo_texto = texto.replace("Rust", "Programação");

println!("{}", novo_texto);
```

A saída será: "Olá, mundo! Meu nome é Programação." Como podemos ver, a função "replace" substituiu todas as ocorrências da palavra "Rust" pela nova palavra.

Também podemos utilizar expressões regulares para realizar buscas e substituições mais complexas. Por exemplo, podemos usar a biblioteca "regex" para substituir todas as vogais em uma string por asteriscos:

```Rust
extern crate regex;

use regex::Regex;

let padrao = Regex::new("[aeiou]").unwrap();
let novo_texto = padrao.replace_all("This is a text", "*");

println!("{}", novo_texto);
```

A saída será: "Th*s *s * t*xt".

## Deep Dive

As funções de busca e substituição em Rust também são altamente eficientes e otimizadas, graças ao seu sistema de gerenciamento de memória. Essas funções também permitem especificar opções como "case sensitive" ou "global match", tornando a manipulação de texto ainda mais versátil.

Rust também fornece outras bibliotecas de manipulação de texto, como "Textwrap" para formatar parágrafos e "Unicode Normalization" para trabalhar com caracteres Unicode.

Além disso, é possível criar expressões regulares com Rust através da biblioteca "Regex", que oferece uma ampla gama de funções para manipulação de texto usando padrão regex.

## Veja Também

- [Documentação oficial da função "replace"](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Tutorial sobre expressões regulares em Rust](https://docs.rs/regex/1.3.7/regex/#using-regular-expressions)
- [Biblioteca "Textwrap" para formatar texto em Rust](https://docs.rs/textwrap/0.11.0/textwrap/)
- [Biblioteca "Unicode Normalization" para trabalhar com caracteres Unicode em Rust](https://docs.rs/unicode-normalization/0.1.11/unicode_normalization/)