---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:00.895562-07:00
description: "Express\xF5es regulares (regex) em JavaScript s\xE3o padr\xF5es usados\
  \ para combinar sequ\xEAncias de caracteres em strings. Programadores as usam para\
  \ buscar,\u2026"
lastmod: '2024-03-13T22:44:46.952878-06:00'
model: gpt-4-0125-preview
summary: "Express\xF5es regulares (regex) em JavaScript s\xE3o padr\xF5es usados para\
  \ combinar sequ\xEAncias de caracteres em strings. Programadores as usam para buscar,\u2026"
title: "Usando express\xF5es regulares"
weight: 11
---

## O Que & Por Que?

Expressões regulares (regex) em JavaScript são padrões usados para combinar sequências de caracteres em strings. Programadores as usam para buscar, extrair e manipular texto, permitindo operações de processamento de string poderosas com um código conciso.

## Como fazer:

### Correspondência Básica

Para começar, você pode criar um padrão de regex simples e usá-lo para encontrar correspondências em uma string. Aqui, vamos encontrar a palavra "codigo":

```javascript
const str = "Eu amo programar em JavaScript.";
const pattern = /codigo/;
const result = pattern.test(str);
console.log(result); // true
```

### Usando `String.prototype.match()`

Para recuperar um array de correspondências:

```javascript
const matches = str.match(/codigo/);
console.log(matches[0]); // "codigo"
console.log(matches.index); // 10
```

### Busca Global

Para encontrar todas as correspondências, use a flag `g`:

```javascript
const globalMatches = str.match(/o/g);
console.log(globalMatches); // ["o", "o", "o"]
```

### Correspondência Insensível a Maiúsculas

A flag `i` ignora o case:

```javascript
const caseInsensitiveMatch = "JavaScript é divertido".match(/javascript/i);
console.log(caseInsensitiveMatch[0]); // "JavaScript"
```

### Substituindo Texto

Use `String.prototype.replace()` para substituir partes da string:

```javascript
const newStr = "JavaScript é divertido".replace(/divertido/, "incrível");
console.log(newStr); // "JavaScript é incrível"
```

### Usando Grupos

Grupos podem capturar partes do padrão:

```javascript
const groupedPattern = /(\w+) é (\w+)/;
const replaceWithGroups = "JavaScript é divertido".replace(groupedPattern, "$2 é $1");
console.log(replaceWithGroups); // "divertido é JavaScript"
```

### Bibliotecas de Terceiros

Embora as capacidades de regex integradas do JavaScript sejam poderosas, algumas tarefas podem ser simplificadas com bibliotecas como `XRegExp`. Ela oferece sintaxe e flags adicionais, tornando padrões complexos mais legíveis:

```javascript
// Exemplo da biblioteca XRegExp
const XRegExp = require('xregexp');
const str = "Gatos são fantásticos.";
const unicodeWordMatch = XRegExp.match(str, XRegExp('\\p{L}+'), 'all');
console.log(unicodeWordMatch); // ["Gatos", "são", "fantásticos"]
```

Este trecho demonstra o uso de `XRegExp` para combinar todas as palavras Unicode em uma string, mostrando a capacidade da biblioteca de lidar com conjuntos de caracteres estendidos além das capacidades integradas do JavaScript.
