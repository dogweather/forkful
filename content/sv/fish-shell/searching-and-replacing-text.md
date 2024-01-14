---
title:                "Fish Shell: Sökning och ersättning av text"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en vanlig uppgift för många programmerare. Genom att göra detta kan du snabbt och enkelt ändra flera delar av en fil på en gång. Fish Shell erbjuder ett enkelt sätt att genomföra detta, vilket gör arbetsflödet smidigare och mer effektivt.

## Hur man gör

För att söka och ersätta text i Fish Shell, använder du kommandot `sed`. Nedan finns några exempel på hur du kan använda denna funktion:

```
Fish Shell
sed 's/old_text/new_text/g' filename.txt
```

Detta kommer att söka igenom filen "filename.txt" och ersätta alla instanser av "old_text" med "new_text". Notera att "g" står för "global", vilket betyder att det kommer att ersätta alla instanser av den gamla texten. Om du vill att ändringarna ska skrivas till en ny fil istället för den befintliga filen, kan du använda flaggan `-i`.

```
Fish Shell
sed -i 's/old_text/new_text/g' filename.txt
```

Du kan också använda sed för att ta bort text:

```
Fish Shell
sed 's/old_text//g' filename.txt
```

Detta kommer att ta bort alla instanser av "old_text" från filen.

## Djupdykning

Sed är en mycket kraftfull funktion i Fish Shell eftersom den erbjuder möjligheten att använda reguljära uttryck för att söka och ersätta text. Detta gör det möjligt att utföra komplexa och mångsidiga ändringar i en fil.

En annan användbar funktion är möjligheten att använda variabler för att ersätta text. Om du till exempel vill byta ut en viss del av en filnamn, kan du använda variabeln `&` för att repr