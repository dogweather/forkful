---
title:                "Sökning och ersättning av text"
html_title:           "Fish Shell: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Har du någonsin behövt ändra ett ord eller en fras i en textfil men tvingats göra det manuellt genom att gå igenom hela filen? Med Fish Shell's inbyggda funktion för att söka och ersätta text, kan du göra detta snabbt och enkelt direkt i terminalen. Det sparar tid och ansträngning, särskilt när du behöver göra flera ändringar i samma fil.

## Hur man gör

För att söka och ersätta text i en fil med Fish Shell, använd kommandot `sed`. Här är ett exempel på hur man ändrar alla förekomster av "hund" till "katt" i en fil som heter `djur.txt`:

```
fish
sed -i 's/hund/katt/g' djur.txt
```

Detta kommer att ersätta alla förekomster av "hund" med "katt" i `djur.txt` och spara ändringarna direkt i filen med hjälp av `-i` flaggan. Om du vill göra ändringarna men inte spara dem kan du använda kommandot utan flaggan `-i`.

## Djupdykning

Det finns fler växlar och parametrar som du kan använda med `sed` kommandot för att anpassa dina ändringar. Till exempel kan du använda ett regex-uttryck för att söka efter ett mönster istället för en specifik sträng. Du kan också kombinera flera flaggor, som `-i` och `-r` för att göra ändringarna rekursivt i flera filer.

För mer information om hur du kan använda `sed` kommandot för att söka och ersätta text, se Fish Shell's dokumentation [här](https://fishshell.com/docs/current/cmds/sed.html).

## Se även

- [Fish Shell dokumentation](https://fishshell.com/docs/current/)
- [Officiell Reddit-sida för Fish Shell](https://www.reddit.com/r/fishshell/)
- [GitHub repository för Fish Shell](https://github.com/fish-shell/fish-shell)