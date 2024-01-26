---
title:                "Concatenando strings"
date:                  2024-01-20T17:35:07.158914-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenando strings"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Concatenar strings é simplesmente juntar dois ou mais pedaços de texto em um só. Programadores fazem isso para montar mensagens, combinar dados e configurar valores dinâmicos de uma forma prática e eficiente.

## Como fazer:
```Javascript
// Usando o operador de concatenação '+'
let saudacao = "Olá" + ", " + "mundo!";
console.log(saudacao); // Saída: Olá, mundo!

// Usando template literals com backticks
let nome = "João";
let mensagem = `Bom dia, ${nome}!`;
console.log(mensagem); // Saída: Bom dia, João!

// Usando a função concat()
let str1 = "Programar ";
let str2 = "é demais!";
let fraseCompleta = str1.concat(str2);
console.log(fraseCompleta); // Saída: Programar é demais!
```

## Aprofundamento
Historicamente, a concatenação de strings no JavaScript podia ser um pouco traiçoeira especialmente em termos de performance. Antes do ECMAScript 2015 (ES6), o '+' era rei, mas poderia ser lento se usado descuidadamente em loops extensos.

Com o template literals do ES6, não só ganhamos clareza com a sintaxe de interpolação `${}`, mas também melhoria no desempenho em alguns casos. A função `concat()` é outra opção, mas raramente usada hoje em dia devido à sua verbosidade comparada às alternativas.

Detalhes de implementação também são importantes. Concatenar muitas strings pode resultar em problemas de performance porque cada concatenação cria uma nova string, já que strings no JavaScript são imutáveis. Para concatenações maciças, por vezes era utilizado arrays e o método `join()`, minimizando a criação de intermediários. Isso ainda é útil em situações específicas que demandam otimização pesada.

## Veja Também
- MDN Web Docs sobre strings: [MDN Strings](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String)
- Artigo sobre templates literais: [MDN Template Literals](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Template_literals)
- Melhores práticas na concatenação de strings: [You Might Not Need jQuery](http://youmightnotneedjquery.com/#concatenate)
