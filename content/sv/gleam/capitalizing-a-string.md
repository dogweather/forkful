---
title:                "Gleam: Att Göra En Sträng Storbokstäver"
simple_title:         "Att Göra En Sträng Storbokstäver"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att omvandla strängar till versaler kan vara en vanlig praxis inom programmering, speciellt när man arbetar med användardata eller skapar utskrifter av text. Med Gleam, en funktionell programmeringsspråk som bygger på Erlang-virtuella maskinen, kan du enkelt konvertera en sträng till versaler med hjälp av en inbyggd funktion. Men varför skulle du vilja göra det? Låt oss ta en titt på några möjliga användningsområden för att omvandla strängar till versaler.

## Hur man gör

För att omvandla en sträng till versaler i Gleam, behöver du bara använda funktionen `String.uppercase`. Här är ett exempel på hur du kan använda den:

```Gleam
let output = String.uppercase("hej värld")
```

I detta exempel kommer strängen "hej värld" att konverteras till "HEJ VÄRLD" och lagras i variabeln `output`. Du kan också använda funktionen på en variabel där en sträng finns lagrad:

```Gleam
let input = "Hej Värld"
let output = String.uppercase(input)
```

Detta kommer också att producera samma resultat. Om du vill se hur funktionen fungerar i praktiken, kan du prova den på en Gleam playground (länk till Gleam playground).

## Djupdykning

Att omvandla strängar till versaler kan vara användbart när du behöver jämföra den omedelbart. Till exempel kan du omvandla användarinmatningar till versaler och sedan jämföra dem med sparad data för att kontrollera om de matchar eller inte, oberoende av om de använde versaler eller inte.

En annan användning kan vara vid skapandet av utskrifter av text. Genom att omvandla en sträng till versaler kan du säkerställa att den visas korrekt och enhetligt, oavsett vilken enhet din applikation körs på.

## Se även

- [Gleam Official Website (länk till Gleam officiella hemsida) ]
- [Gleam Documentation (länk till Gleam dokumentation) ]
- [Gleam på GitHub (länk till Gleam GitHub-sida) ]