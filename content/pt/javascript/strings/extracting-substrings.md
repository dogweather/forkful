---
title:                "Extraindo substrings"
aliases: - /pt/javascript/extracting-substrings.md
date:                  2024-01-20T17:46:06.879237-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extraindo substrings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## O Que e Por Quê?
Extrair substrings é pegar partes específicas de uma string, tipo cortar só um pedaço do texto. Programadores fazem isso para manipular e usar informações de formas bem específicas, como validar dados ou formatar saídas.

## Como Fazer:
```javascript
let texto = "Olá, programadores!";
let saudacao = texto.substring(0, 3);
let foco = texto.slice(5, 19);

console.log(saudacao); // Saída: Olá
console.log(foco); // Saída: programadores
```

Em detalhe:
- `substring(start, end)` pega desde a posição `start` até `end - 1`.
- `slice(start, end)` é similar, mas aceita índices negativos para contar do fim da string.

```javascript
let cumprimento = "Boa tarde, mundo!";
let tarde = cumprimento.slice(-13, -7);

console.log(tarde); // Saída: tarde
```

## Mergulho Profundo
No início do JavaScript, só tínhamos `substring`. Depois veio o `slice`, com a capacidade de aceitar índices negativos. Temos também o `substr`, que é considerado obsoleto, então melhor não se apegar. Esses métodos diferem no trato de argumentos negativos e no caso de serem passados valores fora dos limites da string.

Há alternativas modernas como o método `split`, que fragmenta a string usando um separador, retornando um array de substrings. Ou até mesmo uso de expressões regulares para alcançar resultados mais complexos.

A performance de extração de substrings depende do tamanho da string e da implementação específica do JavaScript no navegador ou ambiente que você está rodando o código. Mas para a maioria dos casos cotidianos, você nem vai notar diferença.

## Veja Também
- MDN Web Docs para `substring`: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/substring
- MDN Web Docs para `slice`: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/slice
- Tutorial sobre `split`: https://www.w3schools.com/jsref/jsref_split.asp
- Guia sobre expressões regulares em JS: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions
