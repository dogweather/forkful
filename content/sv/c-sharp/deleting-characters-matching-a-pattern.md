---
title:                "C#: Att ta bort tecken som matchar ett mönster"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Ibland i vår kod behöver vi ta bort karaktärer som matchar ett specifikt mönster. Det kan vara för att rensa data eller för att manipulera strängar. Att kunna ta bort dessa karaktärer är en användbar färdighet för programmerare.

## Hur man gör

Först måste vi definiera det mönster vi vill matcha med hjälp av ett reguljärt uttryck. Detta kan göras med hjälp av klassen `Regex` i .NET-ramverket. Sedan använder vi `Regex.Replace()` metoden för att ersätta alla matchande karaktärer med en tom sträng.

```C#
string str = "Hej123Världen";
string pattern = "[0-9]"; // ta bort alla siffror
string result = Regex.Replace(str, pattern, ""); //resultatet blir "HejVärlden"
```

För att ta bort specifika karaktärer kan vi använda `[ ]` för att ange vilka karaktärer som ska tas bort, till exempel `[aeiou]` för att ta bort vokaler eller `[,.]` för att ta bort kommatecken och punkter.

Vi kan också använda `^` för att ta bort alla karaktärer som inte matchar vårt mönster. Till exempel `[0-9^]` kommer att ta bort alla karaktärer utom siffror.

## Djupdykning

När vi använder reguljära uttryck för att ta bort karaktärer i C# är det viktigt att förstå hur det fungerar under huven. Reguljära uttryck används för att söka efter mönster i textsträngar och kan även användas för att utföra andra strängoperationer, som att ersätta, splitta eller extrahera delar av en sträng.

För att förstå mer om reguljära uttryck och dess syntax, rekommenderar vi att du läser Microsofts dokumentation om [regular expressions in C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions).

## Se även

* [Microsofts dokumentation om regular expressions i C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
* [Tutorial om reguljära uttryck i C#](https://www.c-sharpcorner.com/article/regular-expression-using-c-sharp/)
* [Online verktyg för att testa reguljära uttryck](https://regexr.com/)