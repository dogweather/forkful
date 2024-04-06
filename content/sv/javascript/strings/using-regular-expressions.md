---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:16.468807-07:00
description: "Hur man g\xF6r: F\xF6r att b\xF6rja kan du skapa ett enkelt regex-m\xF6\
  nster och anv\xE4nda det f\xF6r att hitta matchningar i en str\xE4ng. H\xE4r ska\
  \ vi hitta ordet \"code\"."
lastmod: '2024-03-13T22:44:38.283394-06:00'
model: gpt-4-0125-preview
summary: "F\xF6r att b\xF6rja kan du skapa ett enkelt regex-m\xF6nster och anv\xE4\
  nda det f\xF6r att hitta matchningar i en str\xE4ng."
title: "Att anv\xE4nda regulj\xE4ra uttryck"
weight: 11
---

## Hur man gör:


### Grundläggande matchning
För att börja kan du skapa ett enkelt regex-mönster och använda det för att hitta matchningar i en sträng. Här ska vi hitta ordet "code":

```javascript
const str = "I love to code in JavaScript.";
const pattern = /code/;
const result = pattern.test(str);
console.log(result); // true
```

### Använda `String.prototype.match()`
För att hämta en array med matchningar:

```javascript
const matches = str.match(/code/);
console.log(matches[0]); // "code"
console.log(matches.index); // 10
```

### Globalt Sök
För att hitta alla matchningar, använd flaggan `g`:

```javascript
const globalMatches = str.match(/o/g);
console.log(globalMatches); // ["o", "o", "o"]
```

### Skiftlägesokänslig matchning
Flaggan `i` ignorerar skiftläge:

```javascript
const caseInsensitiveMatch = "JavaScript är kul".match(/javascript/i);
console.log(caseInsensitiveMatch[0]); // "JavaScript"
```

### Ersätta Text
Använd `String.prototype.replace()` för att ersätta delar av strängen:

```javascript
const newStr = "JavaScript är kul".replace(/kul/, "fantastiskt");
console.log(newStr); // "JavaScript är fantastiskt"
```

### Använda Grupper
Grupper kan fånga delar av mönstret:

```javascript
const groupedPattern = /(\w+) är (\w+)/;
const replaceWithGroups = "JavaScript är kul".replace(groupedPattern, "$2 är $1");
console.log(replaceWithGroups); // "kul är JavaScript"
```

### Tredjepartbibliotek
Även om JavaScripts inbyggda regex-funktioner är kraftfulla, kan vissa uppgifter förenklas med bibliotek som `XRegExp`. Det erbjuder ytterligare syntax och flaggor, vilket gör komplexa mönster mer läsbara:

```javascript
// Exempel med XRegExp-biblioteket
const XRegExp = require('xregexp');
const str = "Katter är fantastiska.";
const unicodeWordMatch = XRegExp.match(str, XRegExp('\\p{L}+'), 'all');
console.log(unicodeWordMatch); // ["Katter", "är", "fantastiska"]
```

Det här avsnittet demonstrerar användning av `XRegExp` för att matcha alla Unicode-ord i en sträng, vilket visar bibliotekets förmåga att hantera utökade teckenuppsättningar utöver JavaScripts inbyggda kapaciteter.
