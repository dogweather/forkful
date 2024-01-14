---
title:                "Gleam: Borttagning av tecken som matchar ett mönster"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett mönster kan vara en användbar funktion i Gleam för att rensa och manövrera data i en viss form.

## Hur man gör

För att ta bort tecken som matchar ett visst mönster kan du använda funktionen `delete_matches` i Gleam. Nedan ser du ett exempel på hur du kan använda den:

```
Gleam
let data = "1,2,3,4,5"

let cleaned_data = String.delete_matches(",", data)

Data.log(cleaned_data)

```

I detta exempel tar vi bort alla kommatecken i den ursprungliga datan och resultatet blir `"12345"`. Genom att använda `delete_matches` kan du enkelt manipulera data för att passa dina behov.

## Djupdykning

Funktionen `delete_matches` tar in två argument - mönstret som ska matchas och den ursprungliga datan. För att göra det ännu mer anpassningsbart kan du också skicka in ett tredje argument som anger vilken del av den matchande datan som ska behållas. Om du till exempel bara vill behålla siffrorna efter kommatecknet kan du använda `delete_matches(",", data, end)`. Då får du resultatet `"2345"`.

Det är också värt att nämna att `delete_matches` har väldigt god prestanda eftersom den använder sig av regex bakom kulisserna.

## Se även

- [Gleam dokumentation](https://gleam.run/documentation/)
- [Regex tutorial på svenska](https://regexone.com/lesson/introduction_abcs)
- [Officiell Gleam Twitter](https://twitter.com/gleam_lang)