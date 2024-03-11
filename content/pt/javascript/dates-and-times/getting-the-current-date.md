---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:45.851729-07:00
description: "Obter a data atual em JavaScript \xE9 uma tarefa fundamental, envolvendo\
  \ a recupera\xE7\xE3o e, possivelmente, a manipula\xE7\xE3o da data e hora de hoje.\
  \ Programadores\u2026"
lastmod: '2024-03-11T00:14:20.708737-06:00'
model: gpt-4-0125-preview
summary: "Obter a data atual em JavaScript \xE9 uma tarefa fundamental, envolvendo\
  \ a recupera\xE7\xE3o e, possivelmente, a manipula\xE7\xE3o da data e hora de hoje.\
  \ Programadores\u2026"
title: Obtendo a data atual
---

{{< edit_this_page >}}

## O Que & Por Que?
Obter a data atual em JavaScript é uma tarefa fundamental, envolvendo a recuperação e, possivelmente, a manipulação da data e hora de hoje. Programadores realizam isso para exibir datas em websites, em aplicações, acompanhar interações do usuário, ou lidar com dados sensíveis ao tempo.

## Como fazer:
No JavaScript puro, o objeto `Date` é usado para trabalhar com datas e horas. Aqui está como você pode obter a data e hora atuais:

```javascript
const currentDate = new Date();
console.log(currentDate); // Saída de exemplo: Sex Abr 14 2023 12:34:56 GMT+0100 (Horário de Verão Britânico)
```

Para exibir apenas a data em um formato mais amigável ao usuário, você pode usar métodos como `toLocaleDateString()`:

```javascript
console.log(currentDate.toLocaleDateString()); // Saída de exemplo: 14/4/2023
```

Para ter mais controle sobre o formato, bibliotecas de terceiros como *Moment.js* ou *date-fns* são muito populares, embora seja bom estar ciente de que Moment.js agora é considerado um projeto legado em modo de manutenção.

Usando *Moment.js*:

```javascript
const moment = require('moment'); // assumindo Node.js ou usando um empacotador de módulos
const formattedDate = moment().format('YYYY-MM-DD');
console.log(formattedDate); // Saída de exemplo: 2023-04-14
```

Com *date-fns*, que enfatiza a modularização permitindo que você importe apenas o que precisa:

```javascript
const { format } = require('date-fns');
const formattedDate = format(new Date(), 'yyyy-MM-dd');
console.log(formattedDate); // Saída de exemplo: 2023-04-14
```

Cada abordagem oferece diferentes níveis de conveniência e flexibilidade para trabalhar com datas em JavaScript, desde o objeto `Date` embutido até capacidades mais sofisticadas de formatação e manipulação disponíveis através de bibliotecas.
