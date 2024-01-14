---
title:                "C#: Beräkning av ett datum i framtiden eller det förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller det förflutna"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför?

Att kunna beräkna ett datum i framtiden eller det förflutna kan vara en användbar funktion vid programmering. Med denna kunskap kan du skapa algoritmer som hjälper dig att förutsäga händelser eller skapa tidsspecifika funktioner i ditt program.

## Så här gör du

```C#
// Kodexempel för att beräkna ett datum i framtiden
DateTime today = DateTime.Today; // Skapar ett objekt för dagens datum
int daysToAdd = 30; // Anger antal dagar att lägga till
DateTime futureDate = today.AddDays(daysToAdd); // Beräknar ett datum 30 dagar framåt
Console.WriteLine("Datumet 30 dagar framåt är: " + futureDate); // Skriver ut resultatet
```
```C#
// Kodexempel för att beräkna ett datum i det förflutna
DateTime today = DateTime.Today; // Skapar ett objekt för dagens datum
int daysToSubtract = 14; // Anger antal dagar att subtrahera
DateTime pastDate = today.AddDays(-daysToSubtract); // Beräknar ett datum 14 dagar bakåt
Console.WriteLine("Datumet 14 dagar tillbaka är: " + pastDate); // Skriver ut resultatet
```

Resultat för kodexemplen ovan:
```
Datumet 30 dagar framåt är: 30/3/2021
Datumet 14 dagar tillbaka är: 2/3/2021
```

Att beräkna datum i svenska format kan göras genom att använda `ToString()` metoden med en formatsträng. Exempelvis: `Console.WriteLine(futureDate.ToString("dd/MM/yyyy"));` för att få datumet i formatet DD/MM/YYYY.

## Djupdykning

När du beräknar ett datum i framtiden eller det förflutna, måste du vara medveten om eventuell förändring av datum och tid beroende på vilken tidszon du befinner dig i. Det är också viktigt att välja rätt formatsträng när du skriver ut datumet för att undvika förvirring eller felaktig tolkning.

Det finns också andra funktioner som kan användas för att manipulera datum, som `AddMonths()` för att lägga till månader istället för dagar, eller `AddYears()` för att lägga till år. Det är viktigt att förstå hur dessa funktioner fungerar och vilken effekt de har på ett datumobjekt.

## Se även

- [Microsoft dokumentation om DateTime Class](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [YouTube Video: Date and Time in C# - Working with DateTime](https://www.youtube.com/watch?v=0ep5zepeP0Q)
- [Java Programming Blog Post (om du vill jämföra med Java)](https://mycodingramblings.wordpress.com/2019/07/25/calculating-future-dates-in-java/)