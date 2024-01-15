---
title:                "Gerando números aleatórios"
html_title:           "Javascript: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que usar números aleatórios em Javascript?

Gerar números aleatórios é uma tarefa comum em muitos projetos de programação. Em Javascript, essa função é especialmente útil para lidar com situações em que é necessário representar uma escolha aleatória ou fornecer uma experiência mais dinâmica para o usuário.

## Como fazer em Javascript

Para gerar um número aleatório em Javascript, podemos usar o método `Math.random()`. Ele retorna um número decimal entre 0 (inclusivo) e 1 (exclusivo). Para obter um número inteiro, basta multiplicar esse valor pelo intervalo desejado e arredondá-lo com o método `Math.floor()`.

```
// Gerando um número inteiro aleatório entre 1 e 10
let numAleatorio = Math.floor(Math.random() * 10) + 1;
console.log(numAleatorio); // Exemplo de saída: 7
```

Outro método útil é `Math.round()`, que arredonda um número decimal para o inteiro mais próximo.

```
// Gerando um número aleatório entre 1 e 100
let numAleatorio = Math.round(Math.random() * 100) + 1;
console.log(numAleatorio); // Exemplo de saída: 54
```

Se quisermos gerar uma escolha aleatória entre uma lista de opções, podemos usar o método `Array[Math.floor(Math.random() * array.length)]`. Por exemplo:

```
// Gerando uma escolha aleatória de cores
let cores = ["vermelho", "azul", "verde", "amarelo"];
let corAleatoria = cores[Math.floor(Math.random() * cores.length)];
console.log(corAleatoria); // Exemplo de saída: amarelo
```

## Mergulho mais profundo

Existem algumas coisas que devemos ter em mente ao gerar números aleatórios em Javascript. Primeiro, o método `Math.random()` não garante uma distribuição uniforme perfeita, então é importante usá-lo com cautela em situações que exijam uma precisão maior. Além disso, é importante lembrar que o resultado sempre será um número decimal entre 0 e 1, portanto, devemos usá-lo juntamente com outros métodos para obter o valor desejado.

A geração de números aleatórios também pode ser útil em jogos, simulações e até mesmo para criar animações aleatórias em websites. Combinando o método `Math.random()` com outros recursos do Javascript, as possibilidades são infinitas.

## Veja também

- [Documentação do método Math.random() (em inglês)](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Tutorial sobre geração de números aleatórios em Javascript (em inglês)](https://www.freecodecamp.org/news/how-to-generate-random-values-with-javascript/)
- [Projeto de animação aleatória em uma página web (em inglês)](https://codepen.io/Anon/pen/wjWNvX?editors=0110)