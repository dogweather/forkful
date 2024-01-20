---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

---

## O Quê & Por quê?
Extrair substrings é uma técnica essencial para manipular strings em JavaScript. Em resumo, extraímos substrings para separar um pedaço de texto de uma string maior, o que pode ser útil para a implementação de funcionalidades como pesquisas de texto e formatação de dados.

## Como fazer:

JavaScript oferece várias maneiras de extrair substrings. As mais comuns são `.substring()`, `.slice()` e `.substr()`. Aqui estão alguns exemplos:

```Javascript
let textoCompleto = "Olá, Mundo da Programação!";

let subTexto1 = textoCompleto.substring(0,3); 
console.log(subTexto1);   // Saída: "Olá"

let subTexto2 = textoCompleto.slice(5, 10);
console.log(subTexto2);   // Saída: ", Mun"

let subTexto3 = textoCompleto.substr(11, 12);
console.log(subTexto3);   // Saída: "Programação!"
```

## Mergulho profundo

Extrair substrings tem raízes na programação desde os primeiros dias, e não apenas em JavaScript. Cada um dos métodos citados acima tem pequenas nuances em sua implementação e uso.

- `.substring(startIndex, endIndex)`: Este método retorna parte da string entre os índices inicial e final. Os índices são baseados em zero. Se omitir o segundo argumento, ele extrairá até o final da string. Se os índices for invertidos, ele os ajustará automaticamente.
- `.slice(startIndex, endIndex)`: Este método é bastante similar ao `.substring()`. A principal diferença é que `.slice()` pode aceitar índices negativos, indicando uma posição começando do final da string.
- `.substr(startIndex, length)`: Este método extrai uma substring do índice inicial por um número especificado de caracteres. Ele também aceita um índice inicial negativo.

É importante ter em mente que todas essas funções não modificam a string original, elas retornam uma nova string.

## Veja também

- Documentação oficial JavaScript MDN para [substring](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/substring), [slice](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/slice), e [substr](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/substr).
- Post no blog Explorando [JavaScript for...of vs for...in Loops](https://www.alura.com.br/artigos/javascript-for-of-vs-for-in)
- Tutorial em vídeo: [JavaScript String Functions](https://www.youtube.com/watch?v=09Bw3_ODGLs)