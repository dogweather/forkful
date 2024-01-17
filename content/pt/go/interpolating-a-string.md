---
title:                "Interpolando uma string"
html_title:           "Go: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Interpolar uma string em Go é uma forma de inserir valores dinâmicos dentro de uma string estática, permitindo que os programadores criem strings personalizadas com informações específicas. É uma ferramenta essencial para tornar as mensagens do seu programa mais claras e úteis para o usuário final.

## Como fazer:

O uso da interpolação de string em Go é simples e pode ser feito usando a sintaxe `fmt.Printf` ou `fmt.Sprintf`. Veja o exemplo abaixo:

```
nome := "Maria"
idade := 28
fmt.Printf("Olá, meu nome é %s e tenho %d anos.", nome, idade)
```

Saída: `Olá, meu nome é Maria e tenho 28 anos.`

## Mergulho profundo:

A interpolação de string é uma técnica comum em várias linguagens de programação e foi introduzida pela primeira vez no C em 1972. Em Go, ela é amplamente usada por sua simplicidade e eficiência em formatar mensagens de saída.

Como alternativa à interpolação de string, os programadores também podem usar a concatenação de string ou substituição de parâmetros, mas a interpolação de string é geralmente mais legível e fácil de usar.

A implementação técnica por trás da interpolação de string em Go envolve o uso de pacotes `fmt` e `strings` para manipular e formatar strings de forma eficiente.

## Veja também:

- [Documentação oficial do pacote fmt em Go](https://golang.org/pkg/fmt/)
- [Tutorial em vídeo sobre interpolação de string em Go](https://www.youtube.com/watch?v=1vxLSB6iMzw)
- [Guia passo a passo de como usar a interpolação de string em Go](https://blog.alexellis.io/golang-template-cheat-sheet/)