---
title:                "Analisando uma data a partir de uma string."
html_title:           "Rust: Analisando uma data a partir de uma string."
simple_title:         "Analisando uma data a partir de uma string."
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Parsing, ou análise sintática, é o processo de transformar uma string em um formato de dados específico. Os programadores frequentemente fazem isso para converter datas em formato de string para o tipo de dados Date, que permite manipulações e operações específicas.

Este processo é importante porque permite que os programadores trabalhem com datas de forma mais eficiente e precisa, sem ter que lidar com a complexidade de strings.

## Como fazer:

```
Rust let data_atual = "31/12/2021".parse::<Date>().unwrap();
println!("{}", data_atual);

// Output: 31 de dezembro de 2021
```

Este é um exemplo simples de como fazer parsing de uma data em Rust. Primeiramente, usamos o método `parse()` para converter a string no tipo de dados Date, e em seguida utilizamos o método `unwrap()` para obter o valor real da data. Então, basta imprimir o resultado.

## Aprofundando:

Antes de existirem linguagens de programação modernas, os programadores precisavam escrever seus próprios algoritmos para fazer parsing de datas. Hoje, existem bibliotecas e funções específicas para realizar esse processo em diversas linguagens, tornando a tarefa muito mais simples e eficiente.

Em Rust, existem diversas bibliotecas, como a `chrono`, que ajudam a fazer parsing de datas de forma eficiente e confiável. Além disso, a linguagem possui recursos embutidos para manipulação de datas, como o tipo `DateTime` e o método `format()`.

Para implementar o processo de parsing de datas, é importante entender a estrutura da string e como ela se relaciona com o formato de dados desejado. Além disso, é fundamental lidar com possíveis erros e exceções que podem ocorrer durante o processo.

## Veja também:

- [Documentação oficial do Rust sobre parsing de datas](https://doc.rust-lang.org/std/primitive.str.html#parsing-numbers)
- [Biblioteca chrono](https://github.com/chronotope/chrono)
- [Artigo sobre parsing de datas em JavaScript](https://levelup.gitconnected.com/guide-to-parsing-dates-in-javascript-547f2a402079) (em inglês)