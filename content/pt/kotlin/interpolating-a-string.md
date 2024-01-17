---
title:                "Interpolando uma string"
html_title:           "Kotlin: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que é e por que é feito?

Interpolar uma string é um recurso que permite aos programadores adicionar variáveis ou expressões dentro de uma string em Kotlin. Isso facilita a criação de strings dinâmicas, que se ajustam de acordo com os valores das variáveis. Os programadores usam esse recurso para tornar seus códigos mais legíveis e concisos, além de facilitar a manutenção e modificação das strings.

## Como fazer:

```Kotlin
val nome = "João"
val idade = 25
val mensagem = "Olá, meu nome é $nome e eu tenho $idade anos."
println(mensagem)
```

Output:
```
Olá, meu nome é João e eu tenho 25 anos.
```

## Profundidade:

A interpolação de string foi introduzida originalmente em linguagens como Ruby e Scala e se tornou uma característica popular no desenvolvimento de aplicações web. Em Kotlin, a interpolação é feita usando a sintaxe `$variável` ou `${expressão}`, podendo ser inserida em qualquer lugar dentro de uma string. Alternativas para interpolação incluem concatenação de strings e formatação de strings usando o método `format()`.

## Veja também:

- [Documentação do Kotlin sobre interpolação de string](https://kotlinlang.org/docs/reference/basic-types.html#string-literals)
- [Artigo da IBM sobre interpolação de string em outras linguagens de programação](https://developer.ibm.com/tutorials/swift-string-formatting/)