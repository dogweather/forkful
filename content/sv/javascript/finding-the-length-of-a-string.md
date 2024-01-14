---
title:                "Javascript: Att hitta längden på en sträng"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hitta längden på en sträng är en viktig färdighet inom Javascript-programmering. Det kan hjälpa dig att effektivt behandla och manipulera textdata i dina program.

## Så här gör du

För att hitta längden på en sträng använder vi den inbyggda metoden .length(). Denna metod kan tillämpas på en sträng-variabel och returnerar antalet tecken i strängen.

```Javascript
// Skapa en variabel med en sträng
var namn = "Kalle";

// Använd .length() metoden för att få längden på strängen
var längd = namn.length;

console.log(längd); // Resultatet blir 5, eftersom "Kalle" består av 5 tecken
```

## Dyk ned i detaljerna

Det finns några saker att tänka på när du använder .length() metoden för att hitta längden på en sträng. För det första är det viktigt att komma ihåg att denna metod returnerar antalet tecken i strängen, inte antalet ord. Detta betyder att om du har flera ord i strängen, inklusive mellanslag, kommer .length() att räkna dem som tecken i längden.

En annan viktig aspekt att tänka på är att specialtecken, som till exempel emotikon eller tecken med accenter, kan räknas som flera tecken i längden. Detta beror på hur ditt program tolkar och lagrar dessa tecken.

## Se även

- [MDN dokumentation om .length()](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Så här använder du .length() för att kontrollera inmatningslängd i en formulärbåda](https://codepen.io/havardfrom/pen/KKaNJeN)