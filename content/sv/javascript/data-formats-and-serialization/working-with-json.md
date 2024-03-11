---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:11.781361-07:00
description: "JSON (JavaScript Object Notation) \xE4r ett l\xE4ttviktigt datautbytesformat,\
  \ enkelt f\xF6r m\xE4nniskor att l\xE4sa och skriva samt f\xF6r maskiner att tolka\
  \ och generera.\u2026"
lastmod: '2024-03-11T00:14:11.714191-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) \xE4r ett l\xE4ttviktigt datautbytesformat,\
  \ enkelt f\xF6r m\xE4nniskor att l\xE4sa och skriva samt f\xF6r maskiner att tolka\
  \ och generera.\u2026"
title: Arbeta med JSON
---

{{< edit_this_page >}}

## Vad & Varför?

JSON (JavaScript Object Notation) är ett lättviktigt datautbytesformat, enkelt för människor att läsa och skriva samt för maskiner att tolka och generera. Programmerare använder det för att lagra och transportera data i webbapplikationer, vilket gör det till ryggraden i moderna API:er och webbtjänsters kommunikation.

## Hur man gör:

### Tolka JSON
För att konvertera en JSON-sträng till ett JavaScript-objekt, använd `JSON.parse()`.

```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const obj = JSON.parse(jsonString);
console.log(obj.name); // Utskrift: John
```

### Omvandla JavaScript-Objekt till Strängar
För att konvertera ett JavaScript-objekt tillbaka till en JSON-sträng, använd `JSON.stringify()`.

```javascript
const user = { name: "Jane", age: 25, city: "London" };
const jsonString = JSON.stringify(user);
console.log(jsonString); // Utskrift: {"name":"Jane","age":25,"city":"London"}
```

### Arbeta med Filer i Node.js
För att läsa en JSON-fil och konvertera den till ett objekt i en Node.js-miljö, kan du använda `fs`-modulen. Det här exemplet antar att du har en fil som heter `data.json`.

```javascript
const fs = require('fs');

fs.readFile('data.json', 'utf-8', (err, data) => {
    if (err) kasta err;
    const obj = JSON.parse(data);
    console.log(obj);
});
```

För att skriva ett objekt till en JSON-fil:

```javascript
const fs = require('fs');
const user = { name: "Mike", age: 22, city: "Berlin" };

fs.writeFile('user.json', JSON.stringify(user, null, 2), (err) => {
    if (err) kasta err;
    console.log('Data skrivet till fil');
});
```

### Tredjepartsbibliotek
För komplexa JSON-operationer kan ramverk och bibliotek som `lodash` förenkla uppgifter, men för grundläggande operationer är ofta infödda JavaScript-funktioner tillräckliga. För storskaliga eller prestandakritiska applikationer kan du överväga bibliotek som `fast-json-stringify` för snabbare JSON-omvandling eller `json5` för tolkning och omvandling med ett mer flexibelt JSON-format.

Tolkning med `json5`:
```javascript
const JSON5 = require('json5');

const jsonString = '{name:"John", age:30, city:"New York"}';
const obj = JSON5.parse(jsonString);
console.log(obj.name); // Utskrift: John
```

Dessa exempel täcker grundläggande operationer med JSON i JavaScript, perfekta för nybörjare som går över från andra språk och som vill hantera data i webbapplikationer effektivt.
