---
date: 2024-01-20 17:51:37.669105-07:00
description: "Como Fazer: A interpola\xE7\xE3o de string n\xE3o \xE9 novidade. Existia\
  \ em linguagens mais antigas, mas o Swift a tornou mais simples e segura. Antes,\
  \ concatenar\u2026"
lastmod: '2024-04-05T21:53:47.258565-06:00'
model: gpt-4-1106-preview
summary: "A interpola\xE7\xE3o de string n\xE3o \xE9 novidade."
title: Interpolando uma string
weight: 8
---

## Como Fazer:
```Swift
let nome = "João"
let idade = 28
let mensagem = "Olá, meu nome é \(nome) e eu tenho \(idade) anos."
print(mensagem)
// Saída: Olá, meu nome é João e eu tenho 28 anos.
```

## Mergulho Profundo:
A interpolação de string não é novidade. Existia em linguagens mais antigas, mas o Swift a tornou mais simples e segura. Antes, concatenar strings e variáveis era fácil de errar e às vezes inseguro, levando a bugs ou falhas de segurança. Alternativas como o `String(format:)` ainda existem, mas interpolação é geralmente mais direta e breve. 

Sob o capô, o Swift converte o que está dentro dos parênteses e barras invertidas `\(valor)` numa representação textual, aplicando o protocolo `CustomStringConvertible` se disponível, para personalizar a conversão.

## Ver Também:
- Documentação oficial da Swift sobre Strings: [Swift Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Protocolo Swift `CustomStringConvertible`: [CustomStringConvertible](https://developer.apple.com/documentation/swift/customstringconvertible)
