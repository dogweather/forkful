---
title:                "Att Göra En Sträng Versal"
html_title:           "Bash: Att Göra En Sträng Versal"
simple_title:         "Att Göra En Sträng Versal"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att kapitalisera en sträng i programmering betyder helt enkelt att ändra alla små bokstäver i en sträng till stora bokstäver. Det är vanligt att göra detta när man behöver argumentera eller jämföra strängar, eftersom vissa programmeringsspråk är fallkänsliga och kommer att skilja på små och stora bokstäver.

## Hur man gör:

För att kapitalisera en sträng i Bash kan du använda kommandot `tr` tillsammans med `[:lower:]` för att byta ut alla små bokstäver till stora bokstäver. Här är ett exempel på hur du kan använda det på terminalen:

```Bash
$ echo "hej alla" | tr '[:lower:]' '[:upper:]'
```

Detta kommer att ge utdata "HEJ ALLA". Du kan också använda andra programmeringsspråk som Python eller Ruby för att utföra detta.

## Djupdykning:

Det är intressant att notera att behovet av att kapitalisera strängar har funnits sedan de tidiga dagarna av datavetenskapen. I det första programmeringsspråket, Fortran, behövde man använda speciella funktioner för att jämföra strängar, eftersom det inte fanns inbyggd funktionalitet för detta. Idag har de flesta moderna programmeringsspråken inbyggda funktioner för att hantera detta, men det är fortfarande viktigt att känna till hur man kapitaliserar strängar när det behövs.

Det finns också alternativ till att använda `tr` för att kapitalisera strängar, till exempel `sed` eller `awk`. Dessutom kan man använda bibliotek eller paket som är speciellt utformade för att arbeta med strängar, vilket kan vara användbart om man ska hantera stora mängder data.

## Relaterade källor:

- [Bash-tr](https://linux.die.net/man/1/tr)
- [Sträng hantering i Python](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Ryuk sträng manipulationsbibliotek för Bash](https://github.com/nmathewson/ryuk)