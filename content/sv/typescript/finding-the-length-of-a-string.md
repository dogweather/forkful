---
title:                "Att hitta längden på en sträng"
html_title:           "TypeScript: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att hitta längden på en sträng är en viktig del av programmering eftersom det tillåter oss att få information om hur lång en rad av tecken är. Detta kan vara användbart vid bearbetning av text, utförande av loopar eller någon annan logik som kräver att man vet längden på en sträng.

## Så här gör du:

TypeScript har en inbyggd funktion som heter `length` som kan användas för att hitta längden på en sträng. Se nedan för ett exempel på hur detta kan användas:

```TypeScript
let namn: string = "Maria";
console.log(namn.length); // Resultat: 5
```

## Djupdykning:

I programmering har vi oftast att göra med text, och det är därför viktigt att kunna hantera strängar på ett effektivt sätt. Innan `length`-funktionen fanns, var det vanligt att använda en `for`-loop för att gå igenom varje tecken i en sträng och räkna antalet. Detta var en tidskrävande process och därför är `length`-funktionen ett effektivare alternativ.

En annan metod för att hitta längden på en sträng är att använda `charCodeAt`-funktionen som returnerar en numerisk representation av varje tecken i en sträng. Genom att räkna antalet värden som returneras kan vi få längden på strängen.

Det är också värt att notera att `length`-egenskapen endast returnerar längden på den synliga delen av en sträng. Om strängen innehåller icke synliga tecken som t.ex. mellanslag eller linjeskift så kommer dessa inte att räknas in i längden.

## Se även:

- [Microsoft TypeScript documentation](https://www.typescriptlang.org/docs/home.html)
- [W3Schools guide on strings in TypeScript](https://www.w3schools.com/TS/TS_strings.asp)
- [String length in JavaScript](https://www.w3schools.com/js/js_string_length.asp)