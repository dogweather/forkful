---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:37:21.288602-07:00
html_title:           "Arduino: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Converter uma data de uma string significa transformar texto formato data (como "01/01/2023") em um objeto de data que o JavaScript pode entender e usar. Programadores fazem isso para manipular datas facilmente - calcular durações, mudar formatos ou comparar eventos temporais.

## Como Fazer:
```javascript
// Usando o objeto Date
const dateString = "2023-01-01T00:00:00Z";
const parsedDate = new Date(dateString);
console.log(parsedDate); // Exibe a data no formato do objeto Date

// Usando a biblioteca moment.js para mais flexibilidade
// Você precisa instalar o moment usando 'npm install moment' antes
const moment = require('moment');
const dateMoment = moment("01-01-2023", "DD-MM-YYYY");
console.log(dateMoment.toString()); // Exibe a data em formato legível
```

## Mergulho Profundo
Historicamente, JavaScript tem tido suas dificuldades com datas. O objeto `Date` padrão é fácil de usar, mas não sem suas peculiaridades, especialmente com fusos horários. Além disso, a string de data precisa estar num formato que o construtor `Date` reconheça, o que nem sempre é o caso.

Por isso, surgiram bibliotecas de terceiros como `moment.js` ou `date-fns` para facilitar a vida dos programadores. Essas bibliotecas lidam com a análise da data (parsing), formatação e manipulação de uma forma mais robusta e com suporte a múltiplos idiomas.

Quanto à implementação, JavaScript interpreta strings de data com base na especificação ISO 8601 (`YYYY-MM-DDTHH:mm:ss.sssZ`). A falta de consistência nos formatos de string fornecidos pode resultar em datas inválidas ou inconsistências entre navegadores, fazendo com que o uso de bibliotecas externas seja frequentemente uma escolha mais segura.

Outra opção moderna é o Luxon, um wrapper construído sobre os `Intl` APIs, trazendo funcionalidades de internacionalização para a formatação e manipulação de datas.

## Veja Também
- Documentação MDN sobre Date: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/
- Date-fns, uma alternativa moderna ao Moment.js: https://date-fns.org/
- Luxon, para trabalhar com datas e horários: https://moment.github.io/luxon/