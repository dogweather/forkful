---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concatenação de Strings em JavaScript: O Que, Por que e Como Fazer

## O Que & Por que?  
Concatenar strings é o processo de unir duas ou mais strings em uma só. Programadores o fazem para manipular dados textuais de forma mais eficiente e personalizada.

## Como Fazer:
Aqui estão algumas maneiras simples de concatenar strings em JavaScript.

```javascript
// Método 1: Operador +
let string1 = "Olá, ";
let string2 = "mundo!";
let resultado = string1 + string2; 
console.log(resultado); // "Olá, mundo!"

// Método 2: Método concat()
resultado = string1.concat(string2); 
console.log(resultado); // "Olá, mundo!"

// Método 3: Template Strings
resultado = `${string1}${string2}`;
console.log(resultado); // "Olá, mundo!"
```

## Mergulho Profundo
Concatenar strings é uma prática tão antiga quanto a própria programação. Nos primórdios da programação, as strings muitas vezes eram concatenadas com o uso de operadores de adição (+) ou funções personalizadas.

Para situações que exigem manipulação mais complexa de strings, há alternativas para considerar além da concatenação tradicional, como os métodos `join()` e `split()`.

A concatenação de strings no JavaScript é otimizada em termos de performance, mas, dependendo do tamanho e do número de strings, pode tornar-se uma tarefa pesada e demorada.

## Veja Também
* [MDN Web Docs: Concatenação de strings](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
* [W3Schools: JavaScript String concat() Method](https://www.w3schools.com/jsref/jsref_concat_string.asp)
* [JavaScript.info: Strings](https://javascript.info/string)

Note: Este artigo não possui uma seção de conclusão.