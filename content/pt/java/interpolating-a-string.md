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

## O que é e porquê?

Interpolação de string é uma técnica usada por programadores em que strings podem ser inseridas ou combinadas dinamicamente em uma outra string. Isso é útil para criar mensagens personalizadas ou adicionar variáveis em textos pré-definidos. Programadores usam interpolando strings para tornar seus códigos mais dinâmicos e flexíveis.

## Como fazer:

Um exemplo simples de interpolação de string em Java é o seguinte:

```Java
String nome = "Maria";
System.out.format("Olá, %s! Seja bem-vinda.", nome);
```

Isso resultará na saída: `Olá, Maria! Seja bem-vinda.`

Outro exemplo útil é em casos onde variáveis precisam ser adicionadas em mensagens de erro. Por exemplo:

```Java
int quantidade = 5;
String produto = "canetas";
System.out.format("Você não pode comprar mais de %d %s.", quantidade, produto);
```

A saída será: `Você não pode comprar mais de 5 canetas.`

## Profundidade:

A interpolação de string é uma técnica antiga que tem sido usada em várias linguagens de programação. Ela é uma alternativa à concatenação de strings, onde as strings são simplesmente combinadas usando o operador `+`. Enquanto a concatenação ainda é amplamente usada, a interpolação de string é considerada mais legível e organizada, especialmente para mensagens mais longas.

Em Java, a interpolação de string é suportada pelo método `format()` da classe `System.out`. Ele é baseado no antigo recurso de formatação de string da classe `Formatter`, mas é mais simples e mais adequado para a interpolação de string.

## Veja também:

- [Documentação oficial da classe System.out](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#out)
- [Explicação detalhada sobre interpolação de string em Java](https://www.baeldung.com/java-string-interpolation)