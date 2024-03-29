---
date: 2024-01-26 04:36:56.454232-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0454 \u043F\u0430\u0440\u0441\u0438\u043D\u0433, \u043C\u0430\u043D\
  \u0456\u043F\u0443\u043B\u044E\u0432\u0430\u043D\u043D\u044F \u0442\u0430 \u0437\
  \u0430\u043F\u0438\u0441 \u0434\u0430\u043D\u0438\u0445 XML \u0437\u0430 \u0434\u043E\
  \u043F\u043E\u043C\u043E\u0433\u043E\u044E \u043F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0443\u0432\u0430\u043D\u043D\u044F. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\
  \u0443\u044E\u0442\u044C XML \u0434\u043B\u044F \u043E\u0431\u043C\u0456\u043D\u0443\
  \ \u0434\u0430\u043D\u0438\u043C\u0438 \u043C\u0456\u0436 \u0440\u0456\u0437\u043D\
  \u0438\u043C\u0438\u2026"
lastmod: '2024-03-13T22:44:48.906943-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML \u043E\u0437\u043D\u0430\
  \u0447\u0430\u0454 \u043F\u0430\u0440\u0441\u0438\u043D\u0433, \u043C\u0430\u043D\
  \u0456\u043F\u0443\u043B\u044E\u0432\u0430\u043D\u043D\u044F \u0442\u0430 \u0437\
  \u0430\u043F\u0438\u0441 \u0434\u0430\u043D\u0438\u0445 XML \u0437\u0430 \u0434\u043E\
  \u043F\u043E\u043C\u043E\u0433\u043E\u044E \u043F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0443\u0432\u0430\u043D\u043D\u044F. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\
  \u0443\u044E\u0442\u044C XML \u0434\u043B\u044F \u043E\u0431\u043C\u0456\u043D\u0443\
  \ \u0434\u0430\u043D\u0438\u043C\u0438 \u043C\u0456\u0436 \u0440\u0456\u0437\u043D\
  \u0438\u043C\u0438\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
---

{{< edit_this_page >}}

## Що і Чому?
Робота з XML означає парсинг, маніпулювання та запис даних XML за допомогою програмування. Програмісти використовують XML для обміну даними між різними системами, для файлів конфігурацій, або при роботі зі стандартами як SOAP, що базуються на XML.

## Як це зробити:
```TypeScript
import { parseString } from 'xml2js';

// Приклад XML
const xml = `<note>
                <to>User</to>
                <from>Author</from>
                <heading>Reminder</heading>
                <body>Не забудьте про зустріч!</body>
             </note>`;

// Парсинг XML в JSON
parseString(xml, (err, result) => {
    if(err) throw err;
    console.log(result);
});

// Припустимо, що парсинг був успішний, вивід може виглядати так:
// { note:
//    { to: ['User'],
//      from: ['Author'],
//      heading: ['Reminder'],
//      body: ['Не забудьте про зустріч!'] } 
}
```

## Поглиблене занурення
XML, або Розширювана Мова Розмітки, існує з кінця 90-х років. Його самоописна природа та формат, придатний для читання людьми, швидко зробили його популярним для різноманітних застосувань, таких як стрічки RSS, управління конфігураціями та навіть формати офісних документів, як Microsoft Office Open XML. Але, порівняно з JSON, він багатослівний, і інтерес до нього почав вщухати. JSON отримав основну увагу для веб-базованих API завдяки своїй легшості та нативній сумісності з JavaScript.

Тим не менше, XML ще не вимер. Він використовується в масштабних корпоративних системах та для стандартів документів, які не перейшли на JSON. Інструменти, як `xml2js` для TypeScript або `lxml` в Python, доводять, що потреба в маніпуляції з XML у програмуванні залишається.

TypeScript не має вбудованої підтримки для XML, як це має місце для JSON. Натомість, ви працюєте з бібліотеками. `xml2js` є прикладом. Він трансформує XML в JSON, роблячи дані легшими для роботи для гуру JavaScript.

## Дивіться також
- [MDN Web Docs про XML](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [xml2js npm пакет](https://www.npmjs.com/package/xml2js)
- [W3Schools XML Туторіал](https://www.w3schools.com/xml/)
