---
title:                "Javascript: Att ta bort tecken som matchar ett mönster"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett mönster kan vara en viktig del av programmering för att filtrera genom data eller korrigera felaktig inmatning.

## Så här

För att ta bort tecken som matchar ett visst mönster kan vi använda en kombination av Regex-mönster och `replace()` funktionen. Se nedan för en enkel kod som tar bort alla siffror från en sträng:

```Javascript
let str = "Jag har 55 äpplen och 3 bananer.";
str = str.replace(/[0-9]/g, '');
console.log(str); // Output: Jag har äpplen och bananer.
```

Vi använder `replace()` funktionen och tilldelar sedan resultatet tillbaka till variabeln `str` för att uppdatera strängen utan siffrorna. Regular Expression `/[0-9]/g` matchar alla siffror i strängen och ersätter dem med en tom sträng (dvs. tar bort dem). Om du vill ta bort ett specifikt tecken kan du använda den exakta nyckeln istället för att använda intervallet som visas ovan, till exempel `/a/g` för att ta bort alla bokstäver "a" i strängen.

## Fördjupning

Att använda Regex för att ta bort tecken som matchar ett mönster ger stor flexibilitet och effektivitet. Det finns flera olika metoder för att använda Regex, till exempel att använda fångargrupper för att behålla vissa delar av en sträng eller att använda negativa intervall för att behålla vissa tecken. Det är också viktigt att förstå skillnaden mellan någon tecken och alla tecken och hur det kan påverka resultatet. Genom att fördjupa dig i Regular Expressions kan du bli en mer effektiv och produktiv programmerare.

## Se även

- https://www.w3schools.com/jsref/jsref_replace.asp
- https://www.w3schools.com/js/js_regexp.asp
- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace