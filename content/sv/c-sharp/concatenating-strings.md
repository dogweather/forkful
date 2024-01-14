---
title:    "C#: Sammanslagning av strängar"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför
Att sammanfoga strängar är en vanlig uppgift inom programmering, oavsett vilket programmeringsspråk du arbetar med. Det är en användbar teknik för att slå samman flera ord eller fraser till en enda sträng, som kan användas exempelvis för att skriva ut text eller användas som inmatning för olika funktioner.

## Hur man gör det
För att sammanfoga strängar i C# används operatorn " + " eller metoden "Concat()" och följer syntaxen "sträng1 + sträng2" eller "Concat(sträng1, sträng2)". Här är ett enkelt exempel:

```C#
string förnamn = "Lisa";
string efternamn = "Svensson";
string fullständigtNamn = förnamn + " " + efternamn;
Console.WriteLine(fullständigtNamn);
```

Output: Lisa Svensson

För mer komplexa scenarier, som att sammanfoga flera variabler till en enda sträng, kan man använda "String.Format()" metoden. Detta ger dig möjlighet att ange index för varje variabel som ska ersättas i strängen. Här är ett exempel:

```C#
string favoritBil = "Tesla";
int antalÄgare = 3;
string text = String.Format("Jag äger {1} {0} bilar.", favoritBil, antalÄgare);
Console.WriteLine(text);
```

Output: Jag äger 3 Tesla bilar.

Man kan också använda "String.Join()" metoden för att lägga till ett skiljetecken mellan varje sträng i en samling. Se följande exempel:

```C#
string[] språk = { "C#", "Java", "Python", "Ruby" };
string text = String.Join(", ", språk);
Console.WriteLine(text);
```

Output: C#, Java, Python, Ruby

## Djupdykning
När man sammanfogar strängar i C#, är det viktigt att komma ihåg att strängar är oföränderliga. Detta innebär att när du väl har skapat en sträng, kan du inte ändra den utan måste skapa en helt ny sträng. Därför kan det vara ineffektivt att använda många operatorer eller metoder för att sammanfoga strängar som behöver ändras ofta.

En annan viktig sak att tänka på är att strängar som innehåller siffror eller andra icke-text tecken kan orsaka problem när de ska sammanfogas. För att undvika detta kan man använda metoden "ToString()" för att konvertera siffror till strängar eller "Escape"-tecken för att hantera specialtecken.

## Se även
- [Official C# String Concatenation Documentation](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/string-comparison)
- [C# Documentation: String Format Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.format?view=net-5.0)
- [C# Documentation: String Join Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.join?view=net-5.0)