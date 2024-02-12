---
title:                "Analisando uma data a partir de uma string"
aliases:
- /pt/javascript/parsing-a-date-from-a-string/
date:                  2024-02-03T19:14:28.428307-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisando uma data a partir de uma string"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que & Por quê?
Analisar uma data de uma string permite que programadores convertam representações textuais de datas em objetos `Date` do JavaScript, facilitando manipulações, comparações e operações de formatação de datas. Este processo é essencial para lidar com entrada de usuários, processamento de dados de bancos de dados ou trabalhando com APIs que comunicam datas em formatos de strings.

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
