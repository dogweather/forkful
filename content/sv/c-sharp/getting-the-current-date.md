---
title:    "C#: Att hämta aktuellt datum"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hämta den aktuella datumet är en vanlig funktion i många program. Det används ofta för att visa när en viss händelse inträffade, eller för att skapa dynamiska datumstämplar. I denna guide kommer vi att gå igenom hur man kan hämta det aktuella datumet i C#.

## Hur man gör

För att hämta det aktuella datumet i C# kan du använda klassen `DateTime`. Detta objekt har en mängd olika metoder och egenskaper som gör det enkelt att arbeta med datum och tid.

Först måste du inkludera `System` namespace i början av ditt program för att kunna använda `DateTime`. Sedan kan du hämta det aktuella datumet genom att skapa ett nytt instans av `DateTime` klassen:

``` C#
DateTime nu = DateTime.Now;
```

Metoden `Now` returnerar ett DateTime-objekt som representerar det aktuella datumet och tiden.

För att visa det aktuella datumet på ett visuellt sätt, kan du använda metoden `ToString()` tillsammans med en formateringssträng. Till exempel kan följande kod användas för att skriva ut det aktuella datumet i formatet "år-månad-dag":

``` C#
DateTime nu = DateTime.Now;
Console.WriteLine(nu.ToString("yyyy-MM-dd"));
```

Detta kommer att skriva ut "2021-04-15" om det är den 15 april 2021.

## Djupdykning

`DateTime` klassen innehåller även många andra metoder och egenskaper för att hantera datum och tid. Till exempel kan du använda `AddDays()` metoden för att lägga till ett visst antal dagar till ett datum, eller `Subtract()` metoden för att subtrahera en viss tidsperiod från ett datum.

Du kan också använda `DateTime.TryParse()` metoden för att försöka konvertera en sträng till ett DateTime-objekt. Om konverteringen lyckas, kommer resultet att lagras i ett DateTime-objekt, annars kommer den att returnera `false`.

Det finns också flera olika formateringssträngar som du kan använda med `ToString()` metoden för att visa datumet på olika sätt. Du kan hitta en lista över dessa formateringssträngar [här](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings).

## Se även

- [DateTime-klass](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [DateTime-metoder och egenskaper](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0#methods)
- [Formatsträngar för datum och tid](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)