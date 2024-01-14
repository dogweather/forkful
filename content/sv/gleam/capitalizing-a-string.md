---
title:                "Gleam: Kapitalisering av en sträng"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Varför skulle någon vilja kapitalisera en sträng? Om du någonsin har arbetat med textbehandling eller HTML-formatering så kan du förmodligen inse hur viktig denna funktion kan vara. Genom att kapitalisera en sträng kan du göra den lättare att läsa och mer professionell.

## Hur man gör det

Det är enkelt att kapitalisera en sträng i Gleam. Du behöver bara använda funktionen "String.capitalize" och ange din sträng som argument. Här är ett exempel:

```Gleam 
let str = "hej på dig!" 
let kapitaliseradStr = String.capitalize(str) 
```

Resultatet blir då "Hej på dig!" Det är viktigt att notera att funktionen endast kapitaliserar den första bokstaven i strängen.

## Djupdykning

När vi använder funktionen "String.capitalize" så ändras inte själva strängen utan en ny, kapitaliserad sträng skapas istället. Om du vill ändra den ursprungliga strängen så kan du istället använda funktionen "String.capitalize!".

Det finns också ett par andra funktioner som kan användas tillsammans med "String.capitalize" för att få mer kontroll över hur strängen kapitaliseras. Till exempel kan du använda "String.capitalize_words" för att kapitalisera varje ord i en sträng, istället för bara den första bokstaven. Du kan även använda "String.capitalize_sentences" för att också kapitalisera första bokstaven i varje mening.

## Se även

Här är några länkar till andra användbara funktioner för strängmanipulering i Gleam:

- [String.concat](https://gleam.run/core/String.html#concat)
- [String.trim](https://gleam.run/core/String.html#trim)
- [String.split](https://gleam.run/core/String.html#split)

Lycka till med dina strängoperationer i Gleam!  # Slut.