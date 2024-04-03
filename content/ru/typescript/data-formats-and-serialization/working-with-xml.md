---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:10.973910-07:00
description: "\u041A\u0430\u043A \u0434\u0435\u043B\u0430\u0442\u044C: ."
lastmod: '2024-03-13T22:44:44.632923-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 XML"
weight: 40
---

## Как делать:
```TypeScript
import { parseString } from 'xml2js';

// Пример XML
const xml = `<note>
                <to>User</to>
                <from>Author</from>
                <heading>Reminder</heading>
                <body>Не забудьте о встрече!</body>
             </note>`;

// Преобразование XML в JSON
parseString(xml, (err, result) => {
    if(err) throw err;
    console.log(result);
});

// Предположим, преобразование прошло успешно, вывод может выглядеть так:
// { note:
//    { to: ['User'],
//      from: ['Author'],
//      heading: ['Reminder'],
//      body: ['Не забудьте о встрече!'] } 
}
```

## Погружение в детали
XML, или Расширяемый Язык Разметки, существует с конца 90-х. Его самоописываемая природа и удобный для чтения формат сделали его популярным на раннем этапе для различных приложений, таких как RSS-каналы, управление конфигурациями и даже форматы офисных документов, например, Microsoft Office Open XML. Но по сравнению с JSON он многословен, и тенденция меняется. JSON получил признание для веб-базированных API благодаря своей легковесности и нативной совместимости с JavaScript.

Тем не менее, XML не умер. Он используется в крупномасштабных корпоративных системах и для стандартов документов, которые не перешли на JSON. Инструменты вроде `xml2js` для TypeScript или `lxml` в Python доказывают, что потребность в манипуляциях с XML в программировании продолжает оставаться.

TypeScript не имеет встроенной поддержки XML, как это есть для JSON. Вместо этого вы работаете с библиотеками. `xml2js` является одним из примеров. Она преобразует XML в JSON, делая данные более удобными для работы экспертов по JavaScript.

## Смотрите также
- [MDN Веб-документация по XML](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [npm пакет xml2js](https://www.npmjs.com/package/xml2js)
- [W3Schools Учебник по XML](https://www.w3schools.com/xml/)
