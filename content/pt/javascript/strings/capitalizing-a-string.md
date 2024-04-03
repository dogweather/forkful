---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:36.440285-07:00
description: "Capitalizar uma string significa converter o primeiro caractere da string\
  \ para mai\xFAsculo, mantendo os caracteres restantes como est\xE3o. Essa opera\xE7\
  \xE3o \xE9\u2026"
lastmod: '2024-03-13T22:44:46.946175-06:00'
model: gpt-4-0125-preview
summary: "Capitalizar uma string significa converter o primeiro caractere da string\
  \ para mai\xFAsculo, mantendo os caracteres restantes como est\xE3o."
title: Capitalizando uma string
weight: 2
---

## Como fazer:
Em JavaScript, não existe um método integrado para capitalizar strings diretamente, mas é fácil implementar usando métodos básicos de manipulação de strings.

### Usando JavaScript Padrão
```javascript
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello world')); // Saída: "Hello world"
```

### Versão ES6
Com literais de modelo do ES6, a função pode ser escrita de uma maneira mais sucinta:
```javascript
const capitalize = (str) => !str ? '' : `${str[0].toUpperCase()}${str.slice(1)}`;

console.log(capitalize('hello ES6')); // Saída: "Hello ES6"
```

### Usando Lodash
Lodash é uma biblioteca de utilitários de terceiros popular que oferece uma ampla gama de funções para manipular e trabalhar com valores JavaScript, incluindo strings. Para capitalizar uma string usando Lodash:
```javascript
// Primeiro, instale o lodash se você ainda não o fez: npm install lodash
const _ = require('lodash');

console.log(_.capitalize('LODASH example')); // Saída: "Lodash example"
```
_Observe como Lodash não apenas capitaliza a primeira letra, mas também converte o resto da string para minúscula, o que difere um pouco da implementação em JavaScript puro._

### Usando CSS (Apenas para Fins de Exibição)
Se o objetivo é capitalizar o texto para exibição na UI, o CSS pode ser usado:
```css
.capitalize {
  text-transform: capitalize;
}
```
```html
<div class="capitalize">hello css</div> <!-- Exibe como "Hello css" -->
```
**Nota:** Esse método altera a aparência do texto na página da web sem alterar a string em si em JavaScript.
