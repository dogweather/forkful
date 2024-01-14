---
title:    "C#: Stor bokstavering av en sträng"
keywords: ["C#"]
---

{{< edit_this_page >}}

# Varför

Att kapitalisera en sträng är en vanlig operation inom programmering och kan vara användbart när man vill ändra utseendet på en sträng eller för att följa visuella konventioner. Det är också en bra övning för nybörjare för att lära sig grundläggande programmeringskoncept som loopar och villkorliga uttryck.

# Så här gör man

Kapitalisering av en sträng innebär att ändra första bokstaven i varje ord till versal, och resterande bokstäver till gemener. Detta kan åstadkommas på olika sätt beroende på programmeringsspråk, men i C# kan det göras med hjälp av "ToUpper" och "ToLower" metoder.

En grundläggande kod för att kapitalisera en sträng i C# skulle se ut så här:

```C#
// Skapar en variabel för strängen som ska kapitaliseras
string strängAttKapitalisera = "exempel sträng";

// Delar upp strängen i en array baserat på mellanslag
string[] ord = strängAttKapitalisera.Split(' ');

// Loopar igenom varje ord och kapitaliserar första bokstaven
// Samt sätter resterande bokstäver till gemener
for (int i = 0; i < ord.Length; i++)
{
    ord[i] = char.ToUpper(ord[i][0]) + ord[i].Substring(1).ToLower();
}

// Slår samman arrayen till en sträng igen
string kapitaliseradSträng = string.Join(" ", ord);

// Skriver ut resultatet
Console.WriteLine(kapitaliseradSträng);
// Output: Exempel Sträng
```

Det finns också andra sätt att kapitalisera en sträng i C#, som att använda en "CultureInfo" för att hantera specifika tecken i olika språk eller använda "StringBuilder" för effektivare hantering av minnet.

# Djupdykning

Att kapitalisera en sträng kan ibland verka som en enkel operation, men det finns flera faktorer som kan påverka hur resultatet blir. En av dessa faktorer är skillnader i bokstavsuppsättningar för olika språk. Till exempel kan det vara svårt att korrekt kapitalisera en sträng på svenska eftersom vissa bokstäver, som "Å" och "Ö", inte har tydliga motsvarigheter i gemena och versala bokstäver.

En annan faktor är hantering av specialtecken som mellanslag eller interpunktion. Vissa programmeringsspråk har inbyggda funktioner för att hantera dessa, medan andra kräver att man implementerar dem själv.

# Se även

- [String.ToUpper Method (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=netcore-3.1)
- [String.ToLower Method (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=netcore-3.1)
- [CultureInfo Class (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=netcore-3.1)
- [StringBuilder Class (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=netcore-3.1)