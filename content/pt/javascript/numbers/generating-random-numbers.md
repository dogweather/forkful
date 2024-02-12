---
title:                "Geração de números aleatórios"
aliases: - /pt/javascript/generating-random-numbers.md
date:                  2024-01-27T20:34:34.615838-07:00
model:                 gpt-4-0125-preview
simple_title:         "Geração de números aleatórios"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Gerar números aleatórios em JavaScript é uma técnica usada para criar imprevisibilidade em aplicações, desde jogos que precisam de comportamento aleatório do inimigo até algoritmos de segurança que requerem aleatoriedade criptográfica. Essa capacidade é crucial para desenvolver experiências dinâmicas para o usuário e aplicativos seguros.

## Como Fazer:

### Geração Básica de Número Aleatório

A maneira mais direta de gerar um número aleatório em JavaScript é usar `Math.random()`. Esta função retorna um número pseudo-aleatório de ponto flutuante no intervalo de 0 (inclusivo) a 1 (exclusivo).

```javascript
let randomNumber = Math.random();
console.log(randomNumber);
```

### Gerando um Número Aleatório Dentro de um Intervalo

Frequentemente, você vai querer um número inteiro aleatório dentro de um intervalo específico. Isso pode ser alcançado escalonando e arredondando a saída de `Math.random()`.

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandomInt(1, 100));
```

### Números Aleatórios Criptograficamente Seguros

Para aplicações que requerem um grau mais alto de aleatoriedade (por exemplo, operações criptográficas), o método `crypto.getRandomValues()` pode ser utilizado. Isso fornece aleatoriedade criptográfica, ao contrário dos números pseudo-aleatórios gerados por `Math.random()`.

```javascript
(function generateSecureRandom() {
  let array = new Uint32Array(1);
  window.crypto.getRandomValues(array);
  console.log(array[0]);
})();
```

## Aprofundamento

Historicamente, a geração de números aleatórios em JavaScript era inteiramente dependente da função `Math.random()`. Embora conveniente para a maioria dos casos de uso casuais, seu algoritmo, tipicamente uma variante de um gerador de números pseudo-aleatórios (PRNG) como o Mersenne Twister, não oferece segurança criptográfica.

A introdução da Web Cryptography API trouxe o método `crypto.getRandomValues()`, oferecendo uma maneira de gerar números que são muito menos previsíveis e adequados para aplicações sensíveis à segurança. Este método acessa fontes de aleatoriedade do sistema operacional subjacente, como `/dev/random` em Unix/Linux, que são mais robustas e adequadas para operações criptográficas.

É crucial escolher o método certo para a tarefa em questão. `Math.random()` é suficiente para necessidades básicas como jogos simples, animações, ou qualquer caso onde a qualidade da aleatoriedade não é crítica. No entanto, para recursos de segurança, como tokens de redefinição de senha ou qualquer operação criptográfica, `crypto.getRandomValues()` é a melhor escolha devido à sua superior qualidade de aleatoriedade.

Notavelmente, `Math.random()` gera números com um viés conhecido na maioria das implementações, o que significa que alguns números são mais propensos a ocorrer do que outros. Embora esse viés seja mínimo e muitas vezes imperceptível para aplicações gerais, ele desqualifica `Math.random()` de ser usado em qualquer contexto criptográfico ou aplicações onde a justiça é crítica, como jogos de azar online.

Em conclusão, enquanto as funções internas do JavaScript para gerar números aleatórios cobrem uma ampla gama de necessidades, entender as diferenças e limitações de cada método é essencial para sua aplicação apropriada.
