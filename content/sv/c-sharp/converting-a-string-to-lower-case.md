---
title:                "Omvandla en sträng till gemener"
html_title:           "C#: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till små bokstäver i C# handlar om att göra alla bokstäver i en sträng till små bokstäver istället för att ha en blandning av både små och stora. Detta är vanligtvis användbart när man jämför strängar för att undvika missmatchningar på grund av bokstavsstorlek.

## Hur:
Det finns flera sätt att konvertera en sträng till små bokstäver i C#. Här är tre exempel:

```C#
// Exempel 1: Använda ToLower() metod
string str1 = "Hej jag är en STRÄNG";

// Omvandla strängen till små bokstäver
str1 = str1.ToLower();

// Output: hej jag är en sträng
Console.WriteLine(str1);

// Exempel 2: Använda ToLowerInvariant() metod
string str2 = "Hej jag är en STRÄNG";

// Omvandla strängen till små bokstäver baserat på invariant culture
str2 = str2.ToLowerInvariant();

// Output: hej jag är en sträng
Console.WriteLine(str2);

// Exempel 3: Använda String.ToLower() metod
string str3 = "Hej jag är en STRÄNG";

// Omvandla strängen till små bokstäver och spara det i en ny sträng
string str3Lower = str3.ToLower();

// Output: hej jag är en sträng
Console.WriteLine(str3Lower);
```

## Djupdykning:
Historiskt sett, i äldre versioner av C#, fanns det bara en metod, ToLower(), för att konvertera en sträng till små bokstäver. Denna metod fungerade på att bryta ner en sträng till en lägre bokstavsstorlek baserat på den aktuella kulturen. Senare, när behovet av att konvertera en sträng till små bokstäver utan att påverka kulturen uppkom, lades en annan metod till: ToLowerInvariant(), som gör omvandlingen baserat på en invariant kultur.

Det finns även en tredje metod, String.ToLower(), som är en instansmetod som returnerar en ny sträng istället för att modifiera den befintliga. Detta kan vara användbart om du vill behålla den ursprungliga strängen oförändrad.

## Se även:
Här är några länkar till andra relevanta resurser och information om att konvertera en sträng till små bokstäver i C#:

- [Microsoft Docs: ToLower()](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- [Microsoft Docs: ToLowerInvariant()](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant)
- [Microsoft Docs: String.ToLower()](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)