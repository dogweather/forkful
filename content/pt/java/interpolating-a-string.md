---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O Que é e Por Quê?
Interpolação de strings é um processo no qual inserimos variáveis diretamente em strings. Os programadores fazem isso para tornar o código mais legível e fácil de entender.

## Como Fazer:
Vamos ver um exemplo ao usar a interpolação de strings no Java 15 com a formatação `String::formatted`.

```Java
var nome = "João";
var idade = 23;
var exemplo = "Olá, meu nome é %s e tenho %d anos.".formatted(nome, idade);
System.out.println(exemplo);
```

Neste programa, a saída será: `Olá, meu nome é João e tenho 23 anos.`

## Mergulho Profundo
Na interpolação de strings, inicialmente as linguagens de programação, como Perl e Ruby, levaram a vantagem. No entanto, apenas recentemente, o Java introduziu esse recurso na versão 15.

Existem alternativas, como String.format() ou MessageFormat.format() que são usadas para realizar a interpolação em versões anteriores do Java.

A implementação da interpolação de strings no Java é feita através da formatação de strings e não da interpolação literal de string como em outras linguagens.

## Veja Também
- Documentação oficial da Oracle: [String.format()](https://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html)
- Artigo sobre interpolação de strings no Java: [Baeldung: A Guide to Java String Interpolation](https://www.baeldung.com/java-string-interpolation)