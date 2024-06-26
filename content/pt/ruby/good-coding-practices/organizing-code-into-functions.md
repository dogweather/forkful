---
date: 2024-01-26 01:11:57.050308-07:00
description: "Como fazer: Imagine que voc\xEA est\xE1 escrevendo um script r\xE1pido\
  \ para cumprimentar usu\xE1rios."
lastmod: '2024-03-13T22:44:47.100823-06:00'
model: gpt-4-1106-preview
summary: "Imagine que voc\xEA est\xE1 escrevendo um script r\xE1pido para cumprimentar\
  \ usu\xE1rios."
title: "Organizando o c\xF3digo em fun\xE7\xF5es"
weight: 18
---

## Como fazer:
Imagine que você está escrevendo um script rápido para cumprimentar usuários:

```Ruby
def cumprimentar(nome)
  "Olá, #{nome}!"
end

puts cumprimentar("Alice")   # Saída: Olá, Alice!
puts cumprimentar("Bob")     # Saída: Olá, Bob!
```

Ou talvez você esteja calculando a área de um círculo:

```Ruby
def area_circulo(raio)
  Math::PI * raio ** 2
end

puts area_circulo(5)   # Saída: 78.53981633974483
```

Mais organizado e fácil de lidar, certo?

## Aprofundamento
O conceito de funções, também conhecido como métodos em Ruby, não é novidade — é tão antigo quanto a programação em si. Retornando aos anos 1950, as sub-rotinas, como eram conhecidas, foram introduzidas para reduzir a redundância.

Alternativas? Claro, você tem o código inline, poderia adotar a POO com classes e objetos, ou até mesmo funcional com lambdas e procs. Mas as funções são a base de um código ordenado. Quer desempenho? Variáveis locais em funções são rápidas e as funções podem retornar valores imediatamente com `return`.

Em termos de implementação, você pode definir uma função com `def` e terminá-la com `end`. Você pode definir parâmetros padrão, usar operadores splat para funções variádicas e mais. As funções podem ser tão simples ou complexas quanto você desejar.

## Veja também
- [Documentação de métodos do Ruby](https://ruby-doc.org/core-2.7.0/Method.html)
- [Aprenda a Programar por Chris Pine](https://pine.fm/LearnToProgram/)
- [Practical Object-Oriented Design in Ruby por Sandi Metz](https://www.poodr.com/)
