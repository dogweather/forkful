---
title:    "Javascript: Gerando números aleatórios"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios?

Há muitas situações em que é necessário gerar números aleatórios em um programa Javascript. Isso pode ser útil em jogos, sorteios, testes de desempenho ou qualquer outra aplicação que requeira valores imprevisíveis.

## Como fazer

Para gerar números aleatórios em Javascript, podemos usar a função `Math.random()` que retorna um número aleatório entre 0 (inclusivo) e 1 (exclusivo). Podemos então multiplicar esse valor pelo intervalo desejado e adicionar um número base para obter um número aleatório dentro de um determinado intervalo.

Por exemplo, se quisermos gerar um número aleatório entre 1 e 10, podemos usar o seguinte código:

```javascript
let randomNum = Math.random() * 9 + 1;
console.log(randomNum); // output: um número aleatório entre 1 e 10
```

Outra abordagem é usar a função `Math.floor()` para arredondar o número gerado para baixo e, em seguida, adicionar o número base. Isso nos permitirá obter apenas números inteiros dentro do intervalo desejado.

Por exemplo, se quisermos gerar um número aleatório entre 1 e 10, podemos usar o seguinte código:

```javascript
let randomNum = Math.floor(Math.random() * 10) + 1;
console.log(randomNum); // output: um número aleatório entre 1 e 10
```

Outro caso comum é gerar um número aleatório de uma lista de valores pré-selecionados. Podemos fazer isso atribuindo os valores a um array e, em seguida, gerando um número aleatório que será usado como índice para esse array.

Por exemplo, se quisermos gerar um número aleatório de uma lista de cores, podemos usar o seguinte código:

```javascript
let colors = ["vermelho", "azul", "amarelo", "verde"];
let randomIndex = Math.floor(Math.random() * 4);
let randomColor = colors[randomIndex];
console.log(randomColor); // output: uma cor aleatória da lista
```

## Mergulho profundo

A geração de números aleatórios em Javascript usa um algoritmo baseado em um valor inicial chamado `seed`. Esse valor é utilizado para calcular uma "semente" que será usada para gerar os números aleatórios. O valor padrão dessa semente é obtido a partir do tempo atual, mas podemos definir manualmente o valor da semente usando a função `Math.seedrandom()`.

É importante notar que a função `Math.random()` não é verdadeiramente aleatória, já que usa um algoritmo para gerar os números. No entanto, para a maioria dos casos, esse algoritmo é suficientemente complexo para produzir números que são considerados aleatórios o suficiente.

## Ver também

- [Documentação oficial do Math.random()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Artigo sobre geração de números aleatórios em Javascript](https://www.devmedia.com.br/como-gerar-numeros-aleatorios-em-javascript/37411)
- [Tutorial sobre como gerar números aleatórios em Javascript](https://www.w3schools.com/js/js_random.asp)