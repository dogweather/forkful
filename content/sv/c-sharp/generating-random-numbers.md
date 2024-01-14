---
title:                "C#: Skapa slumpmässiga tal"
simple_title:         "Skapa slumpmässiga tal"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Varför generera slumpmässiga nummer?

I programmering finns det många gånger ett behov av att skapa slumpmässiga nummer, antingen för att testa en algoritm eller för att skapa variation i ett spel. Genom att använda inbyggda funktioner kan vi enkelt generera slumpmässiga nummer i vårt program. I denna bloggpost kommer jag att visa hur du kan göra detta i C#.

## Så här gör du:

För att generera slumpmässiga nummer i C# använder vi oss av den inbyggda klassen Random. Det första steget är att skapa en instans av denna klass genom att skriva ```C# Random randomGenerator = new Random();``` Därefter kan vi använda olika metoder för att generera olika typer av slumpmässiga nummer.

### Heltal:
```C#
// Genererar ett slumpmässigt heltal mellan 1 och 100
int randomNumber = randomGenerator.Next(1, 101);

// Genererar ett slumpmässigt heltal mellan 0 och 100
int randomNumber = randomGenerator.Next(101);

// Genererar ett slumpmässigt heltal utan övre gräns
int randomNumber = randomGenerator.Next();
```

### Decimaltal:
```C#
// Genererar ett slumpmässigt decimaltal mellan 0 och 1
double randomDecimal = randomGenerator.NextDouble();

// Genererar ett slumpmässigt decimaltal mellan 0 och 10
double randomDecimal = randomGenerator.NextDouble() * 10;

// Genererar ett slumpmässigt decimaltal utan övre gräns
double randomDecimal = randomGenerator.NextDouble();
```

### Tecken:
```C#
// Genererar ett slumpmässigt tecken från en lista av tecken
char randomChar = (char)randomGenerator.Next('a', 'z');

// Genererar ett slumpmässigt tecken från hela ASCII-tabellen
char randomChar = (char)randomGenerator.Next(0, 255);
```

## Djupdykning:

Intern använder klassen Random en algoritm som kallas för **Pseudo Random Number Generator (PRNG)**. Det betyder att de genererade numren egentligen inte är helt slumpmässiga utan följer en förutbestämd ordning baserat på ett så kallat seed-värde. Om vi inte anger ett seed-värde när vi skapar vår instans av Random kommer det att använda systemklockan som seed-värde, vilket ger en godtycklig startpunkt för den interna algoritmen.

Ett problem med PRNG är att det kan uppstå en periodisk sekvens av nummer, vilket betyder att de genererade numren kommer att upprepa sig själva efter en viss tid. För att undvika detta kan vi ange ett eget seed-värde när vi skapar vår instans av Random. Detta kan till exempel göras genom att använda programtiden som seed-värde, vilket ger en högre grad av slumpmässighet.

## Se också:

- [Microsoft Docs - Random Class](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0)
- [Wikipedia - Pseudo Random Number Generator](https://sv.wikipedia.org/wiki/Pseudoslumpgenerator)