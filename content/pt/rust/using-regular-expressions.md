---
title:                "Usando expressões regulares"
html_title:           "Rust: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O que e por que?

Expressões regulares, ou "regex", são sequências de caracteres especiais usadas para procurar, substituir ou verificar padrões em strings. Programadores as utilizam para facilitar a busca e manipulação de texto, economizando tempo e aumentando a eficiência do código.

## Como usar:

```
let texto = "A linguagem de programação Rust é incrível!";
let padrao = Regex::new(r"Rust").unwrap();
println!("Regex encontrou a palavra \"{}\" no texto!", padrao.find(texto).unwrap().as_str());

// Output:
// Regex encontrou a palavra "Rust" no texto!
```

Neste exemplo, definimos uma variável com um texto e outra com o padrão que queremos procurar. Em seguida, usamos o método `.find()` para procurar o padrão na string e imprimir o resultado. Para utilizar Regex em seu código, você precisará importar a biblioteca `regex` em seu arquivo.

## Mergulho Profundo:

O conceito de expressões regulares não é novo, ele remonta aos anos 50 e 60. No entanto, foi no final dos anos 80 que elas foram padronizadas e amplamente utilizadas pelos programadores. Existem várias alternativas às expressões regulares, como o uso de métodos de string, mas a conveniência e poder das regex as tornam uma ferramenta valiosa no arsenal de um programador.

A implementação de expressões regulares em Rust é fornecida pela biblioteca `regex`, que usa a sintaxe padrão de regex, mas também tem suas próprias peculiaridades. É importante ter cuidado ao utilizar regex, pois elas podem ser complexas e difíceis de depurar.

## Veja também:

- [Documentação da biblioteca `regex` para Rust](https://docs.rs/regex/1.3.9/regex/)