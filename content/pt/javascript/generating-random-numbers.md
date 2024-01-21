---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:49:35.246324-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Gerar números aleatórios é como tirar um número da cartola digital - você nunca sabe o que vai sair. Programadores fazem isso principalmente para jogos, simulações e tudo o que exige um elemento de surpresa ou variações imprevistas.

## Como Fazer:

```javascript
// Gerar um número aleatório entre 0 (inclusivo) e 1 (exclusivo)
let numeroAleatorio = Math.random();
console.log(numeroAleatorio);

// Gerar um número inteiro aleatório entre dois valores, min e max
function gerarInteiroAleatorio(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

let dado = gerarInteiroAleatorio(1, 6);
console.log(dado);
```

Output:
```
0.123456789
4
```

## Aprofundando:

A função `Math.random()` é um clássico do JavaScript, mas nem sempre foi tão direta. Em tempos antigos, as sementes para aleatoriedade eram germinadas manualmente. Agora, o JavaScript faz a maior parte do trabalho pesado, mas para aplicações com necessidades de segurança elevada, `Math.random()` pode não ser o ideal porque não é criptograficamente seguro. Nesses casos, podem-se utilizar alternativas como a Web Crypto API.

A geração de números aleatórios sempre terá um toque de ironia, afinal, o nosso intuito é simular o imprevisível dentro de sistemas que adoram previsibilidade - os computadores. No universo JavaScript, poderíamos também utilizar bibliotecas de terceiros que adicionam funcionalidades ou melhoram a geração de números aleatórios, como a `chance.js` ou `random-js`.

## Veja Também:

- Documentação da Mozilla sobre `Math.random()`: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Math/random
- Web Crypto API: https://developer.mozilla.org/pt-BR/docs/Web/API/Web_Crypto_API
- Chance.js: http://chancejs.com
- Random-js: https://github.com/ckknight/random-js