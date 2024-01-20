---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att hämta det aktuella datumet är processen att få den exakta tiden just nu från systemklockan. Det är relevant för programmerare eftersom det används i många situationer, som att tidsstämpla databasposter eller mäta prestanda.

## Hur man gör:

Man kan enkelt hämta det aktuella datumet i Typescript med `Date`-objektet. Här är ett exempel där vi skapar ett nytt `Date`-objekt och loggar det till konsolen:

```TypeScript
let nuvarandeDatum = new Date();
console.log(nuvarandeDatum);
```

När du kör den här koden kommer output att se ut något i stil med:

```
2022-03-12T12:34:56.789Z
```

## Djupdykning

Hämta det aktuella datumet har varit en del av programmering språk sedan början. I Typescript, och dess överordnade språk JavaScript, görs detta genom `Date`-objektet, som är en del av det inbyggda objektet API.

Det finns alternativ till `Date`-objektet, som `moment.js`, som erbjuder mer flexibilitet och bättre hantering av datum och tider.

En viktig detalj att notera: när man skapar ett nytt `Date`-objekt utan några argument, kommer det att returnera det aktuella datumet och tiden enligt systemets tidszon. Men, `Date`-objektet omvandlar sedan detta till en ISO-sträng (som vi såg i koden ovan), vilket representerar datum och tid i UTC.

## Se även

För mer information eller alternativa metoder, se följande resurser:
- MDN web docs on the Date object: [Link](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Moment.js documentation: [Link](https://momentjs.com/)
- An article about date and time handling in JavaScript: [Link](https://javascript.info/date)