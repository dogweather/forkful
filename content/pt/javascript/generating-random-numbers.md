---
title:                "Javascript: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios?

A geração de números aleatórios é uma técnica essencial em programação para gerar informações de forma aleatória e imprevisível. Esses números podem ser usados em jogos, sorteios, criptografia e muitos outros contextos. Aprender a gerar números aleatórios em Javascript pode ajudar a adicionar um nível extra de imprevisibilidade e dinamismo em seus projetos.

## Como fazer

Gerar números aleatórios em Javascript é muito simples. Na verdade, a linguagem tem um método embutido para isso: `Math.random()`. Este método retorna um número aleatório decimal entre 0 e 1. Para obter um número inteiro entre um determinado intervalo, podemos usar alguns cálculos matemáticos. Por exemplo, para gerar um número inteiro entre 1 e 10, podemos multiplicar `Math.random()` por 10 e adicionar 1 ao resultado:

```javascript
let numeroAleatorio = (Math.random() * 10) + 1;
console.log(numeroAleatorio); // saída esperada: um número aleatório entre 1 e 10
```

Podemos usar esse conceito para gerar um número aleatório em um intervalo específico, como entre 50 e 100, basta ajustar os valores na fórmula. Além disso, também é possível usar a função `Math.floor()` para arredondar o número para baixo e obter um número inteiro. Por exemplo:

```javascript
let numeroAleatorio = Math.floor((Math.random() * 50) + 50);
console.log(numeroAleatorio); // saída esperada: um número aleatório entre 50 e 100
```

## Mergulho Profundo

Agora que sabemos como gerar números aleatórios em Javascript, é importante entender que esses números não são realmente aleatórios. Eles são chamados de "pseudoaleatórios", o que significa que eles são gerados por um algoritmo e, portanto, são previsíveis. Isso pode não ser um problema para uso em jogos ou sorteios, mas pode ser uma vulnerabilidade em aplicações de segurança, como criptografia. Por isso, é importante usar bibliotecas de terceiros que implementam algoritmos mais complexos para gerar números verdadeiramente aleatórios.

Outro ponto importante é o cuidado ao usar `Math.random()` em iterações, pois isso pode levar à criação de padrões previsíveis nos números gerados. Por exemplo:

```javascript
for(let i = 0; i < 10; i++) {
  console.log(Math.random());
}
```

Neste loop, cada execução irá gerar um número diferente, mas se executarmos várias vezes, podemos identificar padrões nos números gerados.

## Veja também

- [Documentação do método Math.random() em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Biblioteca javascript para geração de números aleatórios verdadeiramente aleatórios](https://github.com/ckrug/random-js)