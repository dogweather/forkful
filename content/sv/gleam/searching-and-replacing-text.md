---
title:                "Söka och ersätta text"
html_title:           "Gleam: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför 
Letar du efter ett enkelt och effektivt sätt att söka och byta ut text i ditt program? Då kan Gleam vara svaret på dina böner! Med Gleam kan du enkelt söka och ersätta text i dina filer, vilket sparar tid och minskar risken för mänskliga fel.

## Hur man gör 
För att söka och ersätta text i Gleam, använd följande kodblock:

```Gleam
Text.replace(pattern, replacement, text)
```

Här är ett exempel på hur du kan använda denna funktion:

```Gleam
Text.replace("Hejsan", "Hallå", "Hejsan världen!")
```

Detta skulle ge följande utmatning:

```Gleam
"Hallå världen!"
```

En annan användbar funktion är ```Text.replace_all```, som tillåter dig att byta ut alla förekomster av ett mönster i en sträng. Här är ett exempel på hur man skulle göra detta:

```Gleam
Text.replace_all("l", "ll", "hello")
```

Detta skulle ge följande utmatning:

```Gleam
"hellllo"
```

## Djupdykning 
Gleam erbjuder även andra funktioner för sökning och ersättning, till exempel möjligheten att söka och byta ut text baserat på ett reguljärt uttryck. För mer information om dessa funktioner och hur man använder dem, se Gleams officiella dokumentation.

## Se även 
- Officiell dokumentation för Gleam: https://gleam.run/
- Reguljära uttryck: https://regexr.com/
- Mer om textbehandling i Gleam: https://gleam.run/docs/std-lib-text/