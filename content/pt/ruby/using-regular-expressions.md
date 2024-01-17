---
title:                "Utilizando expressões regulares"
html_title:           "Ruby: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O que e Por Que?

Usar Expressões Regulares (ou Regex) é uma forma eficiente de trabalhar com expressões de texto específicas em um programa. Isso permite que programadores possam buscar e manipular dados de maneira precisa e rápida. 

## Como fazer:

Vamos ver alguns exemplos simples de como usar Expressões Regulares em Ruby!

```
texto = "Uma string complexa com muitos números: 123-456-789"
```
```
/123-456-789/.match(texto)
 # => #<MatchData "123-456-789">
```

Neste exemplo, estamos usando o método `match` para buscar dentro da string um padrão que inclua os números 123-456-789. Quando encontramos uma correspondência, é retornado uma instância de `MatchData`, que contém as informações sobre a correspondência encontrada.

```
/frutas/i.match("Adoro bananas e maçãs")
 # => #<MatchData "bananas">
```

Neste outro exemplo, a flag `i` é usada para tornar a expressão case-insensitive. Isto significa que podemos buscar palavras como "Maçãs" ou "maçãs" e ambas irão ser correspondidas.

## Mergulho Profundo:

Expressões Regulares têm uma longa história na computação e são suportadas em diversas linguagens de programação, incluindo Ruby. Enquanto alternativas como `String#split` e `String#slice` podem ser usadas para manipulação de strings, Expressões Regulares oferecem uma forma mais poderosa e versátil de buscar e manipular dados.

Por baixo dos panos, Ruby usa a biblioteca `Oniguruma` para implementar Expressões Regulares. Esta biblioteca é escrita em C e é altamente otimizada para lidar com padrões complexos de texto.

## Veja também:

- [Documentação Oficial do Ruby sobre Expressões Regulares](https://ruby-doc.org/core-2.7.2/Regexp.html)
- [Artigo sobre Expressões Regulares no Medium](https://medium.com/factory-mind/regex-tutorial-a-simple-cheatsheet-by-examples-649dc1c3f285)
- [Livro "Mastering Regular Expressions" por Jeffrey E.F. Friedl](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)