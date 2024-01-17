---
title:                "Sökning och ersättning av text"
html_title:           "Bash: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Söka och ersätta text är en vanlig uppgift för programmerare. Det innebär helt enkelt att byta ut en viss text med en annan i ett textdokument. Detta kan vara användbart för att snabbt göra massiva ändringar eller för att automatisera upprepade uppgifter.

## Hur man gör:

För att söka och ersätta text i Bash, använd kommandot `sed` (stream editor). Det tar två huvudsakliga parametrar: "söktermen" och "ersättningstermen." Här är ett exempel på hur man använder det:

Bash kod block:

```
sed 's/hund/katt/g' namnlista.txt
```

Detta kommer att söka igenom filen `namnlista.txt` och ersätta alla instanser av "hund" med "katt." Notera att "g" står för "global," vilket innebär att det kommer att genomföra ersättningen för varje förekomst av söktermen.

Om du vill spara resultatet till en ny fil istället för att visa det direkt i terminalen, kan du använda rörledningar (`|`) och utdataomdirigering (`>`). Till exempel:

Bash kod block:

```
sed 's/hund/katt/' namnlista.txt > nya_namnlista.txt
```

Detta kommer att söka igenom `namnlista.txt` och ersätta "hund" med "katt," och spara den nya filen som `nya_namnlista.txt`.

## Djupdykning:

`sed` är en stream editor som ursprungligen utvecklades för Unix-system på 1970-talet. Det är en mycket kraftfull och mångsidig kommando, men det finns också andra kommandon som kan användas för att söka och ersätta text, som till exempel `awk` och `perl`.

När du använder `sed` i Bash, är det viktigt att lägga märke till att söktermen och ersättningstermen måste vara uttryckta i form av "reguljära uttryck." Detta innebär att det finns vissa tecken och uttryck som har speciell betydelse och måste vara noga utformade. Om du är osäker på hur man skriver ett reguljärt uttryck, finns det många online resurser och guider tillgängliga.

## Se även:

- [Bash Dokumentation](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)
- [Kommandozeile](https://www.kommandozeile.de/)