---
title:                "Organizando o código em funções"
aliases:
- pt/ruby/organizing-code-into-functions.md
date:                  2024-01-26T01:11:57.050308-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizando o código em funções"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Organizar o código em funções divide seu script em partes reutilizáveis. É tudo sobre tornar o código limpo, gerenciável e menos propenso a erros. Código modular é fantástico porque economiza tempo, mantém sua sanidade e simplifica a depuração e os testes unitários.

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
