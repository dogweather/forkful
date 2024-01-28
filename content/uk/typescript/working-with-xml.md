---
title:                "Робота з XML"
date:                  2024-01-26T04:36:56.454232-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з XML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/working-with-xml.md"
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
