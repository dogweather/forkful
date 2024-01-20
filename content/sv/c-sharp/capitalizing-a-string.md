---
title:                "Gör en sträng storstilad"
html_title:           "C#: Gör en sträng storstilad"
simple_title:         "Gör en sträng storstilad"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad och varför?

Att göra om en sträng till versaler innebär att förvandla alla tecken i strängen till stora bokstäver. Programmerare gör detta för att förbättra läsbarheten och för att få jämförelser mellan strängdata att bli okänsliga för utseendet på tecknen.

## Hur gör man:

Nedan är ett exempel på hur du kan använda C# för att förvandla en sträng till versaler.

```C#
string SträngInMatning = "hej världen!";
string SträngUtMatning = SträngInMatning.ToUpper();
Console.WriteLine(SträngUtMatning);
```
Utskriften blir:

```C#
"HEJ VÄRLDEN!"
```

## Mer i detalj

Historiskt sett användes text i versaler för att visa makt och auktoritet. I programmering används det nu för att förbättra läsbarheten och göra kod mer robust genom att göra det enklare att jämföra strängar. Ett alternativ till att använda metoden ToUpper() är att skapa en slinga som går igenom varje tecken i strängen och konverterar det individuellt. Implementationen av metoden ToUpper() är dock ofta effektivare och ger mer läsbar kod.

## Se även

För mer information om hur du använder versaler i C#, se Microsofts officiella dokumentation:

[Microsoft .NET dokumentation om ToUpper()](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=netcore-3.1)

Och här är ett bra inlägg på StackOverflow om ämnet:

[StackOverflow diskussion om att göra om en sträng till versaler](https://stackoverflow.com/questions/4673398/how-do-i-make-all-text-upper-case-in-c-sharp)

Observera att det är viktigt att tänka på att metoden ToUpper() kan bete sig olika beroende på aktuell kulturinställning.