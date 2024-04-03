---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:04.144975-07:00
description: "Gerar n\xFAmeros aleat\xF3rios \xE9 uma tarefa fundamental na programa\xE7\
  \xE3o que \xE9 usada para uma mir\xEDade de aplica\xE7\xF5es, como simula\xE7\xF5\
  es, jogos e sistemas de\u2026"
lastmod: '2024-03-13T22:44:46.102667-06:00'
model: gpt-4-0125-preview
summary: "Gerar n\xFAmeros aleat\xF3rios \xE9 uma tarefa fundamental na programa\xE7\
  \xE3o que \xE9 usada para uma mir\xEDade de aplica\xE7\xF5es, como simula\xE7\xF5\
  es, jogos e sistemas de seguran\xE7a."
title: "Gerando n\xFAmeros aleat\xF3rios"
weight: 12
---

## Como fazer:
No Google Apps Script, você pode gerar números aleatórios usando a função `Math.random()`, semelhante ao JavaScript. Esta função retorna um número pseudo-aleatório de ponto flutuante no intervalo de 0 (inclusivo) a 1 (exclusivo). Para adaptar esses números para vários casos de uso, como gerar inteiros dentro de um intervalo específico, pode ser necessário realizar cálculos adicionais.

### Gerando um Número Aleatório Básico
Para gerar um número aleatório simples e registrá-lo no console:

```javascript
function generateRandomNumber() {
  var randomNumber = Math.random();
  Logger.log(randomNumber);
}
```
*Saída de amostra:* `0.1234567890123456`

### Gerando um Inteiro Dentro de um Intervalo Específico
Para gerar um inteiro aleatório entre dois valores (`min` e `max`), inclusivo:

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  var randomNumber = Math.floor(Math.random() * (max - min + 1)) + min;
  Logger.log(randomNumber);
  return randomNumber;
}

// Exemplo:
getRandomInt(1, 10);
```
*Saída de amostra:* `7`

Lembre-se, a função `Math.ceil()` é usada para arredondar o valor mínimo para cima, e `Math.floor()` é usada para arredondar o valor máximo para baixo, garantindo que o número aleatório esteja dentro do intervalo especificado.

## Mergulho Profundo
O mecanismo para gerar números aleatórios no Google Apps Script, e de fato na maioria das linguagens de programação, utiliza um gerador de números pseudo-aleatórios (PRNG). Esta técnica é determinística e depende de um valor inicial, conhecido como a semente, para produzir uma sequência de números que parece aleatória. Embora suficiente para muitas aplicações, é importante notar que números pseudo-aleatórios podem não ser adequados onde alta segurança ou verdadeira aleatoriedade são exigidas, como em aplicações criptográficas.

A verdadeira aleatoriedade pode ser alcançada através de geradores de números aleatórios de hardware ou serviços que geram aleatoriedade a partir de fenômenos naturais. No entanto, para a maioria das necessidades de scripts do dia a dia no Google Apps Script, `Math.random()` é suficiente.

Historicamente, a busca por técnicas mais eficazes de geração de números aleatórios levou ao desenvolvimento de vários algoritmos, com exemplos notáveis sendo o Mersenne Twister e o Gerador Linear Congruencial (LCG). No entanto, dado o alto nível de abstração no Google Apps Script, a maioria dos usuários não precisará implementar esses algoritmos diretamente, mas entender os princípios subjacentes pode ajudar a apreciar a importância e limitações da geração de números aleatórios em seus scripts.
