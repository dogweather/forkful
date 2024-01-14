---
title:                "TypeScript: Ett utdrag av delsträngar"
simple_title:         "Ett utdrag av delsträngar"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att extrahera substrängar är en vanlig uppgift inom programmering när man behöver manipulera och bearbeta textsträngar. Det kan vara användbart när man vill hitta specifika ord eller tecken i en längre text och utföra åtgärder baserat på det.

## Hur man gör

För att extrahera en substräng i TypeScript kan man använda sig av metoden `substring()` på en sträng. Denna metod tar två parametrar, startindex och slutindex för den önskade substrängen.

```TypeScript
// Skapa en sträng
let namn = "Johanna";

// Extrahera en delsträng från index 2 till 5 (0-baserat)
let delstrang = namn.substring(2, 5);
console.log(delstrang); // Output: han
```

Om man bara anger ett startindex för `substring()` så kommer den att extrahera från det indexet till slutet av strängen. Men om man lämnar ut båda parametrarna så kommer hela strängen att returneras.

```TypeScript
// Extrahera från index 3 till slutet av strängen
let delstrang2 = namn.substring(3);
console.log(delstrang2); // Output: nna
```

Man kan också använda sig av `substr()`-metoden som tar ett startindex och en längd som parametrar. Denna metod är användbar om man vet exakt hur många tecken man vill extrahera.

```TypeScript
// Extrahera 3 tecken från index 1
let delstrang3 = namn.substr(1, 3);
console.log(delstrang3); // Output: oha
```

## Djupdykning

En sak att tänka på när man extraherar substrängar är att indexeringen börjar på 0. Det innebär att det första tecknet i en sträng har index 0, det andra tecknet har index 1 och så vidare. Det är viktigt att ha i åtanke för att få rätt del av strängen.

Man bör också vara medveten om att `substring()` och `substr()` är båda immutabla, vilket betyder att de inte ändrar den ursprungliga strängen utan bara returnerar den nya substrängen. Om man vill ändra en sträng på plats så kan man använda sig av `slice()`-metoden som tar samma parametrar som `substring()`, men den ändrar den ursprungliga strängen.

## Se även

- [TypeScript dokumentation om substrängar](https://www.typescriptlang.org/docs/handbook/basic-types.html#string-substrings)
- [MDN webbdokumentation om substrängar](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN webbdokumentation om `slice()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)