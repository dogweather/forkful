---
title:                "C#: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

I många programmingscenarier är det ofta nödvändigt att konvertera en sträng till små bokstäver. Det kan vara för att jämföra strängar utan att oroa dig för skillnader mellan stora och små bokstäver, eller för att presentationen av data ska vara enhetlig. Oavsett orsaken är det en vanlig uppgift för många programmerare.

## Så här gör du

För att konvertera en sträng till små bokstäver i C#, kan du använda en inbyggd metod som heter `ToLower()`. Detta gör det mycket enkelt och effektivt att uppnå det önskade resultatet. Koden nedan visar hur du kan använda metoden på en sträng och sedan skriva ut det nya värdet till konsolen:

```C#
string str = "DAGEN"; // skapar en sträng med stora bokstäver
string lowercaseStr = str.ToLower(); // använder ToLower() för att konvertera till små bokstäver
Console.WriteLine(lowercaseStr); // skriver ut "dagen" till konsolen
```

Du kan också använda en `for`-loop för att loopa igenom varje tecken i strängen och använda `ToLower()` för att konvertera det till små bokstäver. Detta kan vara användbart om du behöver göra ytterligare manipulationer av varje tecken innan det konverteras.

## Djupdykning

När du använder `ToLower()`-metoden, kommer den att konvertera alla bokstäver till deras Unicode-ekvivalenter. Detta betyder att även specialtecken som finns i andra språk kan påverkas. Till exempel kommer bokstaven "Ö" i svenska att konverteras till "ö" i det nya strängvärdet.

Det är också viktigt att notera att `ToLower()`-metoden skapar en ny sträng och returnerar den. Detta innebär att det ursprungliga strängobjektet inte kommer att ändras. Om det är viktigt att behålla en sträng i originalformatet, se till att använda den konverterade strängen eller tilldela den till ett nytt strängobjekt.

## Se även

- Microsoft Dokumentation: [ToLower() Metod](https://docs.microsoft.com/sv-se/dotnet/api/system.string.tolower?view=net-5.0)
- Tutorialspoint: [C# Strings](https://www.tutorialspoint.com/csharp/csharp_strings.htm)