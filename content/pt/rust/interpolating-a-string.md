---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

---

# Interpolação de String em Rust: Guia Rápido para Iniciantes

## O que é & Por quê?

Interpolação de string é o processo de substituir placeholders inseridos numa string por valores de variáveis. Programadores usam isso para unir, formatar e exibir strings de maneira mais legível e eficiente.

## Como fazer:

Em Rust, usamos o macro `format!()` para interpolação de string, como mostrado abaixo:

```Rust
fn main() {
    let nome = "José";
    let idade = 30;
    let frase = format!("Olá, meu nome é {} e eu tenho {} anos.", nome, idade);

    println!("{}", frase);
}
```

Nesse exemplo, `{} são os placeholders substituídos pelos valores das variáveis `nome` e `idade`. O output será: 

`Olá, meu nome é José e eu tenho 30 anos.`

## Deep Dive

Embora a interpolação de string seja comum em muitas linguagens de programação, Rust optou por um macro de formatação em vez da sintaxe de interpolação de string comumente utilizada. Esta escolha faz parte do compromisso do Rust com a segurança da memória e a clareza do código.

Entre as alternativas, em contextos onde o desempenho é essencial, você pode optar por usar o macro `write!()` para evitar a alocação desnecessária de memória, embora seja menos diático.

A implementação da interpolação de string em Rust é baseada em macros de tempo de compilação, que verificam os argumentos em tempo de compilação para oferecer segurança na formatação das strings.

## Veja Também

- [Documentação oficial do Rust sobre macros](https://doc.rust-lang.org/book/ch19-06-macros.html)
- [Formatação de String em Rust](https://doc.rust-lang.org/std/fmt/)
- [Post detalhado sobre String Interpolation em Rust no StackOverflow](https://stackoverflow.com/questions/29483365/what-is-the-syntax-for-string-interpolation-in-rust)

---

Lembre-se sempre, a melhor forma de aprender qualquer recurso de programação é aplicando-o. Então, experimente interpolação de string em Rust e divirta-se codificando!