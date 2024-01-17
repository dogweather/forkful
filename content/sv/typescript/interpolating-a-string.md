---
title:                "Interpolering av en sträng"
html_title:           "TypeScript: Interpolering av en sträng"
simple_title:         "Interpolering av en sträng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad och varför?

Att "interpolera en sträng" innebär att bygga en ny sträng genom att kombinera en eller flera befintliga strängar med variabler eller uttryck. Detta görs ofta för att skapa dynamiska strängar som anpassas beroende på olika förhållanden eller data.

## Så här gör du:

### Exempel 1:

```typescript
let name = "Alice";
console.log(`Välkommen, ${name}!`);
```
**Output:** Välkommen, Alice!

### Exempel 2:

```typescript
let x = 5;
let y = 10;
console.log(`Summan av ${x} och ${y} är ${x + y}`);
```
**Output:** Summan av 5 och 10 är 15

## Djupdykning:

### Historisk bakgrund:

Interpolation av strängar är en funktion som introducerades i ES6-versionen av JavaScript, och den är nu en del av TypeScript-språket. Innan dess var det vanligt att använda konkatenering för att kombinera strängar och variabler.

### Alternativ:

Alternativa sätt att kombinera strängar och variabler är genom att använda metoden `concat()` eller operatorn `+`. Men interpolering av strängar är ett mer läsbart och effektivt sätt att skapa dynamiska strängar.

### Implementation:

I TypeScript kan interpolering av strängar göras genom att använda backticks (`) istället för vanliga ' eller " för att definiera en sträng. Inuti strängen kan variabler och uttryck läggas till inom `${}` som visas i exemplen ovan.

## Se även:

- [Officiell TypeScript dokumentation för stränginterpolation](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-1-4.html#template-strings)
- [W3Schools guide för stränginterpolation i TypeScript](https://www.w3schools.com/js/js_string_interpolation.asp)
- [Stack Overflow inlägg om fördelarna med stränginterpolation jämfört med konkatenering](https://stackoverflow.com/questions/24197428/should-javascript-joins-be-replaced-with-es6-template-strings)