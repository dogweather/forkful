---
title:    "Rust: Buscando e substituindo texto"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por que

Escrever código é uma arte, e assim como qualquer outra forma de arte, é importante manter nosso trabalho organizado e legível. Um dos problemas mais comuns que os programadores enfrentam é a necessidade de fazer alterações em um determinado trecho de código repetidas vezes. Felizmente, a linguagem de programação Rust possui recursos que facilitam a busca e substituição de texto, tornando esse processo muito mais eficiente e menos suscetível a erros.

## Como fazer

Para começar, vamos supor que temos um arquivo de texto chamado "texto.txt" que contém a seguinte frase: "Eu amo programar com Rust". Queremos substituir a palavra "amo" por "adoro". Em Rust, podemos fazer isso usando o método "replace" da biblioteca "string". Aqui está um exemplo de como ficaria o código:

`` `Rust
use std::fs::File;
use std::io::prelude::*;

fn main () {
    let mut file = File::open ("texto.txt")?
    let mut conteúdo = String :: new ();
    arquivo .ler_para_string (& mut conteúdo)?;

    conteúdo = conteúdo.replace ("amo", "adoro");

    arquivo = File::create ("texto.txt")?;
    arquivo .escrever (conteúdo. como_bytes ())?;
}
`` `

Se executarmos esse código, o conteúdo do nosso arquivo "texto.txt" será alterado para: "Eu adoro programar com Rust".

Outra opção é usar o método "replace_all", que substitui todas as ocorrências de uma palavra por outra. Vamos supor que também queremos substituir a palavra "Rust" por "Rustacean". O código ficaria assim:

`` `Rust
conteúdo = conteúdo.replace_all ("Rust", "Rustacean");
```

E o resultado final seria: "Eu adoro programar com Rustacean".

## Mergulho profundo

Além dos métodos "replace" e "replace_all", Rust também possui o método "replace_range", que permite substituir um trecho específico de uma string por outra. Também podemos usar expressões regulares para tornar nosso processo de busca e substituição mais flexível e preciso.

Por exemplo, vamos supor que queremos substituir todas as palavras que começam com a letra "h" por "hello". Poderíamos fazer isso usando uma expressão regular e o método "replace_regex" da biblioteca "regex". O código ficaria assim:

`` `Rust
use regex :: Regex;

conteúdo = Regex :: new (r "h \ w +"). Substituir (conteúdo, "hello");
```

Assim, a palavra "hacker" seria substituída por "helloer", a palavra "hello" seria substituída por "hellohello" e assim por diante.

## Veja também

- [Documentação da biblioteca "string" do Rust] (https://doc.rust-lang.org/std/string/index.html)
- [Documentação da biblioteca "regex" do Rust] (https://docs.rs/regex/1.4.2/regex/)

Espero que este artigo tenha sido útil para ajudar você a entender como fazer buscas e substituições de texto em Rust. Experimente esses métodos e veja como eles podem facilitar sua vida como programador. Até a próxima!