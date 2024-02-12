---
title:                "Geração de números aleatórios"
aliases:
- /pt/typescript/generating-random-numbers/
date:                  2024-01-27T20:35:28.258848-07:00
model:                 gpt-4-0125-preview
simple_title:         "Geração de números aleatórios"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?

Gerar números aleatórios em TypeScript trata de criar valores numéricos imprevisíveis dentro de um intervalo especificado. Programadores utilizam esses dígitos aleatórios para uma variedade de propósitos, como gerar identificadores únicos, simular dados para testes ou adicionar imprevisibilidade a jogos e simulações.

## Como fazer:

Em TypeScript, você pode gerar números aleatórios usando o objeto global `Math`. Abaixo estão alguns exemplos práticos demonstrando como produzir números aleatórios para diferentes requisitos.

### Gerando um Número Aleatório Básico

Para gerar um número decimal aleatório básico entre 0 (inclusivo) e 1 (exclusivo), você usa `Math.random()`. Isso não requer qualquer manipulação adicional:

```TypeScript
const randomNumber = Math.random();
console.log(randomNumber);
```

Isso pode gerar um valor como `0.8995452185604771`.

### Gerando um Número Inteiro Aleatório Entre Dois Valores

Quando você precisa de um inteiro entre dois valores específicos, você incorpora tanto `Math.random()` quanto alguma aritmética:

```TypeScript
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

const randomInt = getRandomInt(1, 10);
console.log(randomInt);
```

Isso pode gerar um valor inteiro entre 1 e 10, como `7`.

### Gerando um Identificador Único

Números aleatórios podem ser combinados com outros métodos para criar identificadores únicos, por exemplo, um simples trecho gerador de UUID:

```TypeScript
function generateUUID(): string {
    return 'xxxxyxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
        const r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

const uuid = generateUUID();
console.log(uuid);
```

Isso gera uma string que se assemelha a um UUID, como `110e8400-e29b-41d4-a716-446655440000`.

## Aprofundamento

O método principal para gerar números aleatórios em JavaScript e, assim, em TypeScript, `Math.random()`, depende de um gerador de números pseudoaleatórios (PRNG). É importante notar que, embora os resultados possam parecer aleatórios, eles são gerados por um algoritmo determinístico baseado em um valor de semente inicial. Portanto, os números produzidos por `Math.random()` não são verdadeiramente aleatórios e não devem ser usados para fins criptográficos.

Para números aleatórios criptograficamente seguros, a Web Crypto API oferece `crypto.getRandomValues()`, que é acessível em ambientes que suportam o padrão Web Crypto, incluindo navegadores modernos e Node.js (via o módulo `crypto`). Aqui está um exemplo rápido ilustrando seu uso em TypeScript para gerar um número aleatório seguro dentro de um intervalo:

```TypeScript
function secureRandom(min: number, max: number): number {
    const array = new Uint32Array(1);
    window.crypto.getRandomValues(array);
    return min + (array[0] % (max - min + 1));
}

const secureRandNum = secureRandom(1, 100);
console.log(secureRandNum);
```

Este método fornece um nível mais forte de aleatoriedade e é mais adequado para aplicações sensíveis à segurança. No entanto, também é mais intensivo em recursos e pode não ser necessário para tarefas mais mundanas, como simulações simples ou geração de valores aleatórios não críticos.
