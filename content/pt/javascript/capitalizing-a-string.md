---
title:                "Capitalizando uma string"
html_title:           "Javascript: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Capitalizar uma string é simplesmente transformar a primeira letra de cada palavra em maiúscula e converter as demais letras em minúsculas. Os programadores o fazem para tornar a string mais legível e seguir uma convenção de escrita padrão.

## Como fazer:

```
 // Exemplo 1:
 let string = "capitalize this string";
 string = string.toLowerCase().replace(/(^|\s)[a-z]/g,function(f){return f.toUpperCase();});
 console.log(string);
// Saída: "Capitalize This String"

// Exemplo 2:
 let string = "I want to capitalize every word";
 let words = string.split(' ');

 for (let i = 0; i < words.length; i++) {
     words[i] = words[i].charAt(0).toUpperCase() + words[i].substring(1).toLowerCase();
 }
 console.log(words.join(' '));
// Saída: "I Want To Capitalize Every Word"

```

## Detalhando mais:

Existem várias maneiras de capitalizar uma string em Javascript, como utilizado nos exemplos acima. No entanto, essa funcionalidade também pode ser alcançada utilizando bibliotecas ou métodos específicos, dependendo do framework utilizado pelo programador. Além disso, é importante notar que a capitalização pode ser aplicada não apenas a letras, mas também a outras partes de uma string, como números e símbolos.

## Veja também:

- [Sitepoint - Quick Tip: Capitalize First Letter of a String in JavaScript](https://www.sitepoint.com/community/t/quick-tip-capitalize-first-letter-of-a-string-in-javascript/890)
- [Stackoverflow - How do I make the first letter of a string uppercase in JavaScript?](https://stackoverflow.com/questions/1026069/how-do-i-make-the-first-letter-of-a-string-uppercase-in-javascript)