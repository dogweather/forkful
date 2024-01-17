---
title:                "Att hitta längden av en sträng"
html_title:           "C#: Att hitta längden av en sträng"
simple_title:         "Att hitta längden av en sträng"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng är en grundläggande del av programmering som innebär att mäta antalet tecken i en given sträng. Detta kan vara användbart för att utföra olika manipulationer på strängen eller för att jämföra den med andra strängar. Detta är en viktig färdighet för alla programmerare att ha i sitt verktygsfack.

## Hur?
Att hitta längden på en sträng i C# är mycket enkelt, tack vare den inbyggda metoden `Length`. Här är ett exempel på hur du kan använda den:

```C#
string myString = "Hej! Hur mår du?";
int length = myString.Length;
Console.WriteLine(length);
```
Output: 15

Som du kan se, returnerar `Length`-metoden antalet tecken i den givna strängen och sparar det i en variabel `length`. Detta värde skrivs sedan ut i konsolen.

## Deep Dive
Att hitta längden på en sträng kanske verkar som en enkel uppgift, men det är faktiskt en viktig del av programmeringen som har en intressant historisk bakgrund. I äldre språk som C, var det vanligt att man behövde ange längden på en sträng manuellt, vilket innebar att man behövde hålla reda på antalet tecken och se till att det stämde överens vid varje manipulation. Med C# och andra moderna programmeringsspråk är detta inte längre nödvändigt, eftersom `Length`-metoden gör det åt dig.

Det finns också alternativ till `Length`-metoden, som `Count`-metoden som används på listor och andra samlingar. Detta kan vara användbart om du vill hitta antalet element i en lista istället för tecken i en sträng.

Om du är intresserad av att veta mer om hur `Length`-metoden faktiskt implementeras, kan du titta på C#-källkoden där det är en enkel returnering av en intern variabel som håller reda på antalet tecken.

## Se även
Om du vill lära dig mer om strängmanipulering och andra grundläggande koncept i C#, kan du kolla in dessa källor:

- Dokumentationen för `Length`-metoden: https://docs.microsoft.com/en-us/dotnet/api/system.string.length

- En tutorial om strängmanipulering i C#: https://www.tutorialspoint.com/csharp/csharp_strings.htm

- Officiella C#-dokumentationen: https://docs.microsoft.com/en-us/dotnet/csharp/