---
title:                "Användning av reguljära uttryck"
html_title:           "Javascript: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Regular expressions, eller Regex, är ett kraftfullt verktyg som används för att söka och manipulera textsträngar i Javascript. Med hjälp av Regex kan du snabbt och enkelt hitta och bearbeta specifika mönster eller tecken i en text.

## Så här gör du

Regex kan användas i Javascript genom att använda inbyggda metoder som "test()" och "match()". För att använda Regex behöver du skriva ett mönster som matchar de tecken eller mönster du letar efter och sedan "matcha" det mot en textsträng.

```Javascript
let myString = "Hej! Mitt namn är Ivan.";
let regex = /Ivan/;
let result = regex.test(myString); // Returnerar true
```

Resultatet av detta "test" kommer att returnera antingen "true" eller "false", beroende på om mönstret matchar textsträngen eller inte.

Du kan också använda Regex för att hitta och ersätta text i en sträng med hjälp av metoden "replace()".

```Javascript
let myString = "Välkommen, [namn]!";
let regex = /[namn]/g;
let result = myString.replace(regex, "Ivan"); // Returnerar "Välkommen, Ivan!"
```

Du kan använda regex för att hitta och extrahera specifika delar av en textsträng genom att använda metoden "match()". Detta är särskilt användbart när man arbetar med data från externa källor som till exempel en API-anrop.

```Javascript
let myString = "Datumet är 02/12/2020";
let regex = /\d{2}\/\d{2}\/\d{4}/g;
let result = myString.match(regex); // Returnerar ["02/12/2020"]
```

Det finns också många olika specialtecken som du kan använda i Regex för att göra dina sökningar mer specifika och avancerade. Till exempel kan du använda "[]" för att matcha en specifik uppsättning tecken, "?" för att matcha ett tecken som kan vara där eller inte och många fler.

## Fördjupning

Att kunna använda Regex är en viktig färdighet för alla Javascript-utvecklare. Det är en effektiv och kraftfull metod för att hantera textsträngar och kan spara dig mycket tid och frustration när du arbetar med data.

Några saker att tänka på när du använder Regex är att det kan vara känsligt för små skillnader i texten, till exempel skillnaden mellan stora och små bokstäver, så se till att ditt mönster är rätt och att du testar det ordentligt innan du använder det i din kod.

Det finns också många resurser tillgängliga på nätet för att hjälpa dig lära dig mer om Regex, inklusive tutorialer, cheat sheets och community-forum där du kan få hjälp och ställa frågor.

## Se även

- [RegExr](https://regexr.com/) - En interaktiv online editor som hjälper dig att testa och lära dig Regex.
- [MDN Regex](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions) - En omfattande guide till Regex på MDN.
- [Regex101](https://regex101.com/) - En annan online editor för att testa och lära dig Regex.