---
title:                "Fish Shell: Formatera en sträng"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Det kan vara viktigt att formatera din kod på ett snyggt och enhetligt sätt för att göra den mer lättläslig och lättförståelig för andra utvecklare. Att använda capitalisering av strängar är en del av denna formatering och kan bidra till en mer professionell kodbas.

## Hur man gör det

För att göra detta i Fish Shell, kan du använda funktionen `string capitalize` tillsammans med en variabel som innehåller din sträng. Exempelvis:

```
set my_string "hej världen!"
echo (string capitalize $my_string)
```

Detta kommer att ge följande utmatning:

```
Hej världen!
```

Funktionen `string.capitalize` tar emot en parameter och returnerar en version av denna parameter med den första bokstaven kapitaliserad.

## Djupdykning

För dem som önskar ytterligare förståelse kan det vara intressant att veta hur denna funktion arbetar bakom kulisserna. I grund och botten konverterar den strängen till ett teckenmatris, ändrar sedan den första bokstaven från lowercase till uppercase och sätter sedan ihop matrisen igen till en sträng.

Detta kan även göras med andra tecken istället för bokstäver. Om man till exempel använder `string capitalize '123hello'`, kommer utmatningen att bli `123Hello`.

## Se även

- [`string capitalize` dokumentation](https://fishshell.com/docs/current/cmds/string.html#string-capitalize)
- [En guide till Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Att automatisera uppgifter med Fish Shell](https://fishshell.com/docs/current/tutorial.