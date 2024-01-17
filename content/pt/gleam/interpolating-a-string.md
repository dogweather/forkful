---
title:                "Interpolando uma string"
html_title:           "Gleam: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Interpolar uma string é a capacidade de combinar diferentes valores em uma string para criar uma frase dinâmica. Os programadores fazem isso para tornar suas mensagens mais personalizadas e úteis para o usuário, tornando o código mais eficiente e legível.

## Como fazer:

```
Gleam.format("Olá, meu nome é {}", "Paula")
> Olá, meu nome é Paula
```
```
let name = "João"
Gleam.format("Olá, {}!", name)
> Olá, João!
```

## Mergulho Profundo:

A interpolação de strings tem sido uma técnica comum em programação há muitos anos, mas com a popularidade crescente de linguagens de programação funcionais, como Gleam, ela se tornou mais padrão e funcional. Além disso, existem outras formas de combinar strings, como concatenação, mas interpolação é geralmente mais eficiente e legível, especialmente quando se trabalha com muitos valores diferentes.

## Veja também:

- [Guia de Referência Gleam: Formatação de Strings](https://gleam.run/reference/formatting_strings)
- [Vídeo Tutorial: Introdução à Interpolação de Strings com Gleam](https://www.youtube.com/watch?v=H4ldYLNxriE)
- [Gleam Playground: Experimente Interpolação de Strings em Ação](https://playgleam.io/?code=let%20name%20%3D%20%22Ana%22%0Alet%20age%20%3D%2032%0A%0A%3E%20Gleam.format(%22Ol%C3%A1%2C%20meu%20nome%20%C3%A9%20%7B%7D%20e%20eu%20tenho%20%7B%7D%20anos.%22%2C%20name%2C%20age))