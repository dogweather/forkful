---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:15:21.249913-07:00
html_title:           "Bash: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta det aktuella datumet i JavaScript innebär att man får veta exakt vilken dag det är just nu, ner till millisekunden. Programmerare gör detta för att hantera tidsstämplar, datumvisa funktioner eller för att enkelt spåra när något inträffar i en app.

## Så här gör du:
För att få det nuvarande datumet och tiden använder du `new Date()`:

```javascript
let nu = new Date();
console.log(nu);
```

Exempelutmatning:

```javascript
2023-03-17T10:42:37.000Z
```

För att endast visa datumet i ett läsbart format kan du använda `toLocaleDateString()`:

```javascript
let dagensDatum = new Date().toLocaleDateString('sv-SE');
console.log(dagensDatum);
```

Exempelutmatning:

```javascript
"2023-03-17"
```

## Djupdykning
JavaScripts `Date` objekt har funnits sedan ECMAScript 1 och är byggt på Java's `java.util.Date` klass. Det använder antalet millisekunder sedan midnatt 1 januari 1970, UTC.

Alternativen inkluderar bibliotek som Moment.js eller Date-fns som erbjuder mer robusta datumhanteringsfunktioner.

När du använder `new Date()`, skapar JavaScript en instans baserad på webbläsarens systemtid. Det innebär att tidszoner och sommartid räknas in. För att hantera tidszoner konsekvent, överväg att använda `toISOString()` eller bibliotek som hanterar tidszoner.

## Se Även
- MDN Web Docs för `Date`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/
- Date-fns biblioteket: https://date-fns.org/
- ISO 8601 Wikipedia: https://en.wikipedia.org/wiki/ISO_8601