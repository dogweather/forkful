---
title:    "C#: Borttagning av tecken som matchar ett mönster"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Det finns många anledningar till varför man skulle vilja ta bort tecken från en sträng som matchar en viss mönster. Det kan vara för att rensa en text från onödiga eller skadliga tecken, eller för att filtrera ut specifika tecken för vidare bearbetning.

## Hur man gör det

För att ta bort tecken från en sträng som matchar ett visst mönster kan man använda sig av metoder från klassen Regex i C#. Här är ett exempel på hur man kan göra det:

```C#
// Skapa en sträng
string text = "Hej, jag heter Anna och jag gillar att programmera!";

// Använda Regex.Replace för att ta bort alla mellanslag
string newText = Regex.Replace(text, " ", "");
Console.WriteLine(newText); 
// output: Hej,jagheterAnnaochjaggillarattprogrammera!
```

I exemplet ovan används metoden Replace från klassen Regex för att ta bort alla mellanslag i strängen. Man kan även använda sig av Regex för att ta bort andra typer av tecken, som exempelvis punkter eller siffror. Det är även möjligt att använda Regex för att filtrera ut specifika tecken enligt ett visst mönster.

## Djupdykning

Regex står för Regular Expressions och är ett kraftfullt verktyg för strängmanipulation och mönstermatchning. Genom att skapa olika mönster kan man hitta och bearbeta olika typer av tecken i en sträng. Det finns även möjlighet att använda så kallade "grupper" för att isolera och ta bort specifika delar av en sträng som matchar ett visst mönster.

En nackdel med att använda Regex är dock att det kan vara svårt att läsa och förstå kod som använder sig av detta. Det är också viktigt att vara försiktig när man skapar mönster, då det kan leda till oanade konsekvenser om man inte tänker på alla möjliga matchningar.

## Se även

- [Microsoft dokumentation om Regex i C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Mer om Regular Expressions i C#](https://www.w3schools.com/cs/cs_regex.asp)
- [En online Regex tester](https://regexr.com/) för att testa och öva på mönstermatchning.