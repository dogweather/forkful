---
title:                "Generera slumpmässiga tal"
html_title:           "C#: Generera slumpmässiga tal"
simple_title:         "Generera slumpmässiga tal"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en användbar funktion inom programmering eftersom det tillåter utvecklare att skapa dynamiska och varierande program. Det är också en viktig funktion inom spel och simuleringar.

## Så här gör du

För att generera slumpmässiga nummer i C# kan du använda dig av "Random" klassen. Här är ett enkelt exempel på hur du kan skapa en instans av Random klassen och sedan generera ett slumpmässigt heltal mellan 1 och 10:

```C#
Random random = new Random();
int randomNumber = random.Next(1, 11);
Console.WriteLine(randomNumber); 
```

Output: 8

För att generera ett slumpmässigt decimaltal mellan 0 och 1 kan du använda "NextDouble()" metoden:

```C#
Random random = new Random();
double randomDecimal = random.NextDouble();
Console.WriteLine(randomDecimal);
```

Output: 0.731447659681

Du kan också ange ett seed-värde för att få samma slumpmässiga sekvens varje gång du kör koden. Detta kan vara användbart vid felsökning eller för att testa olika scenarier:

```C#
Random random = new Random(1234);
int randomNumber = random.Next(1, 11);
Console.WriteLine(randomNumber);
```

Output: 3

## Djupdykning

Random klassen använder sig av en algoritm som kallas "Linear Congruential Generator" för att generera slumpmässiga tal. Algoritmen använder en seed-värde som utgångspunkt för att skapa en följd av tal som kan upplevas som slumpmässiga.

Det är viktigt att vara medveten om att algoritmen inte genererar helt slumpmässiga tal utan följer en specifik matematisk formel. Detta betyder att om du känner till seed-värdet och algoritmen, kan du förutsäga nästa tal som kommer att genereras.

För att få en bättre slumpmässighet kan du använda andra metoder, som till exempel att blanda element i en lista eller använda externa faktorer såsom systemets tid eller användarinput som seed-värde.

## Se även

- [Random Klass (C# Programmeringsguide)](https://docs.microsoft.com/sv-se/dotnet/api/system.random?view=net-5.0)
- [Pseudo-Random Number Generators (Wikipedia)](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [How to Create Random Numbers in C# (Tutorial)](https://www.tutorialspoint.com/how-to-create-random-numbers-in-c-sharp)