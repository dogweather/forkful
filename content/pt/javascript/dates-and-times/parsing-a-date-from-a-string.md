---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:28.428307-07:00
description: "Como fazer: JavaScript oferece nativamente o m\xE9todo `Date.parse()`\
  \ e o construtor `Date` para analisar strings de datas. No entanto, essas abordagens\
  \ t\xEAm\u2026"
lastmod: '2024-03-13T22:44:46.971834-06:00'
model: gpt-4-0125-preview
summary: "JavaScript oferece nativamente o m\xE9todo `Date.parse()` e o construtor\
  \ `Date` para analisar strings de datas."
title: Analisando uma data a partir de uma string
weight: 30
---

## Como fazer:
JavaScript oferece nativamente o método `Date.parse()` e o construtor `Date` para analisar strings de datas. No entanto, essas abordagens têm limitações e inconsistências entre diferentes navegadores, especialmente com formatos de datas não padronizados. Para lidar com esses problemas, bibliotecas de terceiros como `Moment.js` e `date-fns` são populares por sua robustez e facilidade de uso.

### Usando JavaScript nativo:
```javascript
const dateString = "2023-04-30T14:55:00";
const dateObj = new Date(dateString);

console.log(dateObj);  // Saída: Sun Apr 30 2023 14:55:00 GMT+0000 (Horário Universal Coordenado)
```

### Usando Moment.js:
Primeiro, instale o Moment.js via npm ou inclua-o no seu projeto. Então:
```javascript
const moment = require('moment');

const dateString = "2023-04-30T14:55:00";
const dateObj = moment(dateString);

console.log(dateObj.toString());  // Saída: Sun Apr 30 2023 14:55:00 GMT+0000
```

### Usando date-fns:
Após adicionar `date-fns` ao seu projeto, analise uma string de data assim:
```javascript
const { parseISO } = require('date-fns');

const dateString = "2023-04-30T14:55:00";
const dateObj = parseISO(dateString);

console.log(dateObj);  // Saída: 2023-04-30T14:55:00.000Z
```

Tanto `Moment.js` quanto `date-fns` oferecem capacidades de análise mais abrangentes, incluindo o manuseio de uma variedade de formatos e locais, o que os torna preferíveis para aplicações complexas.
