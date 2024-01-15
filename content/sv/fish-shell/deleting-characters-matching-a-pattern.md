---
title:                "Radering av tecken som matchar ett mönster"
html_title:           "Fish Shell: Radering av tecken som matchar ett mönster"
simple_title:         "Radering av tecken som matchar ett mönster"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att kunna ta bort tecken som matchar ett mönster kan vara en användbar funktion när man arbetar med textfiler eller kommandoradsverktyg. Det kan hjälpa till att effektivisera arbetsflödet och spara tid.

## Hur man gör det

Fish Shell har en inbyggd funktion för att ta bort tecken som matchar ett visst mönster. För att använda det behöver du bara skriva "string nomatch -nm 'mönster'" i terminalen.

```Fish Shell
string "Hello World!" nomatch -nm "o"
```

Detta kommer att resultera i "Hell Wrl".

## Djupdykning

Det finns flera olika flaggor som kan användas för att anpassa borttagningen av tecken. Förutom "-nm" som står för "nomatch" finns det också "-eq" för "equal" och "-ne" för "not equal". Det finns också ett antal andra flaggor som kan användas för att specificera ett mer exakt mönster eller påverka borttagningen på andra sätt.

Det är också möjligt att ange ett område av tecken som ska tas bort genom att använda "string nomatch -nm 'mönster[start-end]'" där "start" och "end" är index som anger vilken del av strängen som ska bort. Till exempel kan "string "Hello World!" nomatch -nm "3-7"" ge resultatet "Hel World!".

## Se även

- [Fish Shell dokumentation](https://fishshell.com/docs/current/)
- [Fish Shell GitHub repository](https://github.com/fish-shell/fish-shell)
- [En utförlig guide till Fish Shell](https://hackernoon.com/an-advanced-guide-to-fish-shell-57e14b7dd89f)