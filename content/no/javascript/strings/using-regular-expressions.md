---
title:                "Bruke regulære uttrykk"
aliases:
- no/javascript/using-regular-expressions.md
date:                  2024-02-03T19:17:57.152608-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke regulære uttrykk"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva og hvorfor?

Regulære uttrykk (regex) i JavaScript er mønstre som brukes for å samsvare med tegnkombinasjoner i strenger. Programmerere bruker dem til å søke, utvinne og manipulere tekst, noe som tillater kraftige strengbehandlingsoperasjoner med kortfattet kode.

## Hvordan:

### Grunnleggende samsvarende

For å starte, kan du lage et enkelt regex-mønster og bruke det til å finne samsvarende i en streng. Her vil vi finne ordet "kode":

```javascript
const str = "Jeg elsker å kode i JavaScript.";
const pattern = /kode/;
const result = pattern.test(str);
console.log(result); // true
```

### Bruk av `String.prototype.match()`

For å hente en matrise av samsvarende:

```javascript
const matches = str.match(/kode/);
console.log(matches[0]); // "kode"
console.log(matches.index); // 10
```

### Globalt søk

For å finne alle samsvarende, bruk `g`-flagget:

```javascript
const globalMatches = str.match(/o/g);
console.log(globalMatches); // ["o", "o", "o"]
```

### Samsvarende uten å ta hensyn til bokstavstørrelse

`i`-flagget ignorerer bokstavstørrelse:

```javascript
const caseInsensitiveMatch = "JavaScript er gøy".match(/javascript/i);
console.log(caseInsensitiveMatch[0]); // "JavaScript"
```

### Erstatte tekst

Bruk `String.prototype.replace()` til å erstatte deler av strengen:

```javascript
const newStr = "JavaScript er gøy".replace(/gøy/, "fantastisk");
console.log(newStr); // "JavaScript er fantastisk"
```

### Bruke grupper

Grupper kan fange deler av mønsteret:

```javascript
const groupedPattern = /(\w+) er (\w+)/;
const replaceWithGroups = "JavaScript er gøy".replace(groupedPattern, "$2 er $1");
console.log(replaceWithGroups); // "gøy er JavaScript"
```

### Tredjepartsbiblioteker

Selv om JavaScripts innebygde regex-kapasiteter er kraftige, kan noen oppgaver forenkles med biblioteker som `XRegExp`. Det tilbyr ekstra syntaks og flagg, noe som gjør komplekse mønstre mer lesbare:

```javascript
// XRegExp bibliotekseksempel
const XRegExp = require('xregexp');
const str = "Katter er fantastiske.";
const unicodeWordMatch = XRegExp.match(str, XRegExp('\\p{L}+'), 'all');
console.log(unicodeWordMatch); // ["Katter", "er", "fantastiske"]
```

Dette utsnittet demonstrerer bruken av `XRegExp` for å samsvare alle Unicode-ord i en streng, og viser bibliotekets evne til å håndtere utvidede tegnsett utover JavaScripts innebygde kapasiteter.
