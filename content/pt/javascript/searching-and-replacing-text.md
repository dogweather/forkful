---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O Que e Por Quê?

Buscar e substituir texto é uma prática comum de programação que envolve a localização de um pedaço específico de texto (uma "string") e a substituição por outra. É importante para manipular e corrigir dados, esclarecer variáveis ou otimizar a eficiência do código.

## Como Fazer:

No JavaScript, você pode usar o método `replace()` para buscar e substituir texto. Veja como funciona:

```Javascript
var str = "Olá, sou um programador!";
var newText = str.replace("programador", "desenvolvedor");

console.log(newText);
```

Ao executar este código, a saída será: "Olá, sou um desenvolvedor!" 

## Aprofundando

O método `replace()` tem uma longa história na programação e tem sido uma parte integral do JavaScript desde sua introdução. No entanto, este método tem suas limitações. Ele só substitui a primeira instância do texto, e para substituições mais complexas, você precisaria usar expressões regulares.

Para alternativas, existem diversas bibliotecas de manipulação de strings disponíveis, como a lodash e a underscore, que oferecem maior flexibilidade. Além disso, você também pode usar ciclos de repetição para implementar uma busca e substituição manualmente.

Por último, é importante compreender que o método `replace()` não altera a string original, mas retorna uma nova. Por isso, se você precisa substituir várias ocorrências de um texto, deve usar o método `replaceAll()`, introduzido recentemente ao JavaScript.

```Javascript
let str = 'Olá, sou um programador, programador, programador!';
let newText = str.replaceAll('programador', 'desenvolvedor');

console.log(newText);
```
Ao executar este código, a saída será: "Olá, sou um desenvolvedor, desenvolvedor, desenvolvedor!" 

## Veja Também

Para se aprofundar em manipulação de texto em JavaScript, veja estes recursos:

- Documentação oficial da Mozilla sobre `replace()`: mdn.io/StringReplace
- Documentação oficial da Mozilla sobre `replaceAll()`: mdn.io/StringReplaceAll
- Lições no w3schools sobre manipulação de strings em JavaScript: w3schools.com/js/js_string_methods.asp

E lembre-se, práticar é a chave para aprofundar o conhecimento. Happy coding!