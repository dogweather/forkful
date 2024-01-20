---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Söka och Ersätta Text i Fish Shell

## Vad & Varför?

Sökning och ersättning av text innebär att identifiera specifika sekvenser av tecken och byta ut dessa mot ett annat värde. Detta är oumbärligt för programmerare, eftersom det gör att de snabbt kan ändra variabler och andra textelement på många ställen samtidigt.

## Hur man gör:

Här är ett enkelt exempel på hur du kan söka och ersätta text i Fish Shell. Tänk dig att vi letar efter texten "hej" och vill byta ut den mot "hallå" i en textsträng.
```Fish Shell
set txt "hej världen"
set txt (string replace "hej" "hallå" $txt)
echo $txt
```
Output:
```
hallå världen
```

## Djupdykning

Historiskt sett diskuterades sök- och ersätt-begreppet redan i de tidiga dagarna av programmering, och det återspeglas i att majoriteten av textredigerare och programmeringsspråk erbjuder denna funktionalitet. I Fish Shell utförs sökning och ersättning genom inbyggda strängfunktioner, men andra verktyg som 'sed' och 'awk' kan också användas.

Det är viktigt att notera att när du använder 'string replace' i Fish Shell, så sker sökningen och ersättningen på en global nivå som standard, vilket innebär att alla förekomster av söksträngen kommer att ersättas. Om du bara vill göra en enstaka ersättning, kan du använda '-r' eller '--regex' flaggan.

## Se även

För mer information om hur du arbetar med strängar i Fish Shell, se följande resurser:
- [Fish Shell Documentation: string](https://fishshell.com/docs/current/cmds/string.html)