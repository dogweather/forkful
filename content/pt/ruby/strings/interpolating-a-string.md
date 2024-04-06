---
date: 2024-01-20 17:51:42.187548-07:00
description: "Como Fazer: Interpola\xE7\xE3o de strings existe em Ruby desde suas\
  \ vers\xF5es iniciais, servindo como uma ferramenta conveniente e eficiente para\
  \ combinar texto e\u2026"
lastmod: '2024-04-05T21:53:47.440381-06:00'
model: gpt-4-1106-preview
summary: "Interpola\xE7\xE3o de strings existe em Ruby desde suas vers\xF5es iniciais,\
  \ servindo como uma ferramenta conveniente e eficiente para combinar texto e c\xF3\
  digo."
title: Interpolando uma string
weight: 8
---

## Como Fazer:
```Ruby
nome = "João"
mensagem = "Olá, #{nome}! Tudo bem?"
puts mensagem
# Saída: Olá, João! Tudo bem?

preco = 9.99
quantidade = 2
puts "O total é R$ #{preco * quantidade}"
# Saída: O total é R$ 19.98
```

## Mergulho Fundo:
Interpolação de strings existe em Ruby desde suas versões iniciais, servindo como uma ferramenta conveniente e eficiente para combinar texto e código. Ela só funciona com aspas duplas ou literais de string `%Q`. Alternativas incluem a concatenação de strings com o operador `+` ou `<<`, mas a interpolação é mais performática por evitar a criação de novos objetos string durante o processo. Na interpolação, o que está dentro de `#{}` é avaliado como código Ruby, convertido para uma string e inserido na string maior.

## Veja Também:
- [Documentação oficial de String do Ruby](https://ruby-doc.org/core-2.7.0/String.html)
- [Ruby Style Guide sobre interpolação](https://rubystyle.guide/#string-interpolation)
