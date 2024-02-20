---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:16.468807-07:00
description: "Regulj\xE4ra uttryck (regex) i JavaScript \xE4r m\xF6nster som anv\xE4\
  nds f\xF6r att matcha teckenkombinationer i str\xE4ngar. Programmerare anv\xE4nder\
  \ dem f\xF6r att s\xF6ka,\u2026"
lastmod: 2024-02-19 22:04:57.525399
model: gpt-4-0125-preview
summary: "Regulj\xE4ra uttryck (regex) i JavaScript \xE4r m\xF6nster som anv\xE4nds\
  \ f\xF6r att matcha teckenkombinationer i str\xE4ngar. Programmerare anv\xE4nder\
  \ dem f\xF6r att s\xF6ka,\u2026"
title: "Att anv\xE4nda regulj\xE4ra uttryck"
---

{{< edit_this_page >}}

## Vad & Varför?

Reguljära uttryck (regex) i JavaScript är mönster som används för att matcha teckenkombinationer i strängar. Programmerare använder dem för att söka, extrahera och manipulera text, vilket möjliggör kraftfulla strängbearbetningsoperationer med koncis kod.

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
