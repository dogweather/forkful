---
date: 2024-01-26 01:16:13.565804-07:00
description: "Organizar o c\xF3digo em fun\xE7\xF5es significa dividir seu c\xF3digo\
  \ em blocos reutiliz\xE1veis e modulares. Fazemos isso para manter as coisas DRY\
  \ (Do not Repeat\u2026"
lastmod: 2024-02-19 22:05:05.363140
model: gpt-4-0125-preview
summary: "Organizar o c\xF3digo em fun\xE7\xF5es significa dividir seu c\xF3digo em\
  \ blocos reutiliz\xE1veis e modulares. Fazemos isso para manter as coisas DRY (Do\
  \ not Repeat\u2026"
title: "Organizando o c\xF3digo em fun\xE7\xF5es"
---

{{< edit_this_page >}}

## O Que & Por Que?
Organizar o código em funções significa dividir seu código em blocos reutilizáveis e modulares. Fazemos isso para manter as coisas DRY (Do not Repeat Yourself - Não Se Repita), tornando o código mais limpo, mais fácil de ler e simples de depurar.

## Como fazer:
Imagine que você está fazendo uma calculadora básica. Em vez de escrever a lógica de adição em todos os lugares onde precisa dela, crie uma função `add`:

```TypeScript
function add(x: number, y: number): number {
  return x + y;
}

console.log(add(5, 7)); // Saída de amostra: 12
```

Agora, digamos que precisamos de uma função para multiplicar:

```TypeScript
function multiply(x: number, y: number): number {
  return x * y;
}

console.log(multiply(3, 4)); // Saída de amostra: 12
```
Percebeu como nos concentramos em uma tarefa por função? Esse é o cerne de organizar o código.

## Aprofundando
Historicamente, conforme as linguagens de programação evoluíram, funções tornaram-se vitais na estruturação do código, inspirando-se em funções matemáticas. Elas são um pilar na programação procedural e continuam presentes nos paradigmas de programação orientada a objetos e programação funcional.

Alternativas? Você poderia simplesmente não usar funções, mas isso é um bilhete só de ida para a Cidade do Espaguete. Ou você poderia adotar a POO (Programação Orientada a Objetos) e empacotar funcionalidades em métodos - que são basicamente funções que pertencem a objetos.

Em termos de implementação, o TypeScript insiste em tipos. Definir os tipos de entrada e saída para funções não é apenas uma questão de educação; é um must para um código TypeScript limpo. Além disso, com o TypeScript, você obtém recursos interessantes como sobrecargas, genéricos e parâmetros opcionais para turbinar suas funções.

## Veja Também
Confira estes recursos para aprimorar seu jogo de funções:

- [Manual do TypeScript – Funções](https://www.typescriptlang.org/docs/handbook/2/functions.html): Sua Bíblia para funções TypeScript.
- [Código Limpo JavaScript](https://github.com/ryanmcdermott/clean-code-javascript#functions): Aplique os princípios do Código Limpo às suas funções JavaScript.
- [Você não Sabe JS – Escopo & Closures](https://github.com/getify/You-Dont-Know-JS): Entenda como funções trabalham com escopo e closures em JavaScript.
