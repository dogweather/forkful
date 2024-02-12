---
title:                "Gerando números aleatórios"
aliases: - /pt/google-apps-script/generating-random-numbers.md
date:                  2024-02-01T21:54:04.144975-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gerando números aleatórios"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/google-apps-script/generating-random-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Gerar números aleatórios é uma tarefa fundamental na programação que é usada para uma miríade de aplicações, como simulações, jogos e sistemas de segurança. Os programadores empregam essa técnica no Google Apps Script para introduzir variabilidade, testar cenários e adicionar imprevisibilidade às suas aplicações dentro do ecossistema Google, incluindo Sheets, Docs e Formulários.

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
