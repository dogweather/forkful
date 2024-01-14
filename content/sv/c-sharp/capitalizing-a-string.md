---
title:                "C#: Bokstavera en sträng"
simple_title:         "Bokstavera en sträng"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför
Varför skulle du vilja lära dig hur man stora bokstäver en sträng i C#? Ibland vill man kanske ha en sträng som är lättare att läsa, eller så behöver man anpassa en sträng för ett specifikt syfte. Oavsett anledning, är det alltid bra att ha den här färdigheten i din programmeringsverktygslåda.

## Hur man gör
```C#
// Skapa en sträng
string minSträng = "detta är en sträng som behöver stora bokstäver";

// Använd funktionen ToUpper() för att stora bokstäver strängen
string storaBokstäver = minSträng.ToUpper();

// Skriv ut resultatet
Console.WriteLine(storaBokstäver);

// Output: DETTA ÄR EN STRÄNG SOM BEHÖVER STORA BOKSTÄVER
```

## Djupdykning
Det finns egentligen två sätt att stora bokstäver en sträng i C#. Det första sättet är att använda funktionen ToUpper(), som beskrivs i föregående exempel. Det andra sättet är att använda funktionen ToUpperInvariant(), vilket ger samma resultat men med ett annat tillvägagångssätt. Skillnaden mellan de två är att ToUpperInvariant() använder de inbyggda inställningarna för ditt operativsystem, medan ToUpper() tar hänsyn till de specifika kulturinställningarna för din applikation.

Ibland kanske du också behöver göra om en sträng till små bokstäver istället för stora. I så fall kan du använda funktionen ToLower() eller ToLowerInvariant() för samma resultat fast med små bokstäver istället.

## Se även
- Microsofts dokumentation om ToUpper() function: https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=netframework-4.8
- En guide för C# strängar: https://www.c-sharpcorner.com/UploadFile/a8e6df/string-in-C-Sharp-programming-language/