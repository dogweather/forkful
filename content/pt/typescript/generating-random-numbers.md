---
title:                "Gerando números aleatórios"
html_title:           "TypeScript: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# O que e por que?
Gerar números aleatórios é um processo em que um programa de computador produz um número sem seguir um padrão específico. Os programadores fazem isso para criar um elemento de aleatoriedade em seus programas e jogos, tornando-os mais interessantes e desafiadores.

# Como fazer:
```TypeScript
// Exemplo de geração de um número aleatório entre 1 e 10
const num = Math.floor(Math.random() * 10) + 1;
console.log(num);
// Saída: 7
```
Neste exemplo, usamos o método ```Math.random()``` para gerar um número decimal entre 0 e 1, multiplicamos por 10 e arredondamos para baixo usando ```Math.floor()```, e somamos 1 para obter um número aleatório entre 1 e 10. Este processo pode ser modificado para gerar números aleatórios em outros intervalos.

# Mergulho profundo:
Gerar números aleatórios é uma tarefa importante na programação desde os primórdios da computação. Inicialmente, os programadores usavam algoritmos matemáticos complexos para gerar números aleatórios, mas hoje em dia, a maioria das linguagens de programação oferecem métodos nativos para essa finalidade, como o ```Math.random()``` em TypeScript.

No entanto, é importante lembrar que, apesar de parecerem aleatórios, os números gerados pelos computadores são determinísticos e podem ser previstos por algoritmos matemáticos. Para aumentar a aleatoriedade, pode-se usar dados externos, como a hora do sistema, como semente para o algoritmo gerador.

# Veja também:
- Documentação oficial do ```Math.random()```: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Math/random
- Um artigo interessante sobre a geração de números aleatórios em programação: https://www.geeksforgeeks.org/pseudo-random-number-generator-prng/