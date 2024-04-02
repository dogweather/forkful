---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:16.994809-07:00
description: "Associativa f\xE4lt, eller ordb\xF6cker i C#, l\xE5ter dig lagra och\
  \ hantera par av nycklar och v\xE4rden. De \xE4r ditt f\xF6rstahandsval n\xE4r du\
  \ beh\xF6ver h\xE4mta v\xE4rden\u2026"
lastmod: '2024-03-13T22:44:37.906042-06:00'
model: gpt-4-0125-preview
summary: "Associativa f\xE4lt, eller ordb\xF6cker i C#, l\xE5ter dig lagra och hantera\
  \ par av nycklar och v\xE4rden. De \xE4r ditt f\xF6rstahandsval n\xE4r du beh\xF6\
  ver h\xE4mta v\xE4rden\u2026"
title: "Att anv\xE4nda associativa arrayer"
weight: 15
---

## Vad & Varför?

Associativa fält, eller ordböcker i C#, låter dig lagra och hantera par av nycklar och värden. De är ditt förstahandsval när du behöver hämta värden snabbt baserat på en unik identifierare, vilket gör datahantering till en barnlek i komplexa applikationer.

## Hur man gör:

I C# arbetar du med associativa fält genom att använda klassen `Dictionary<TKey, TValue>`. Här är ett snabbt exempel för att komma igång:

```C#
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        // Skapar en ordbok
        Dictionary<string, int> fruitBasket = new Dictionary<string, int>();

        // Lägger till nyckel-värde-par
        fruitBasket.Add("Apples", 5);
        fruitBasket.Add("Oranges", 10);

        // Åtkomst av ett värde med dess nyckel
        Console.WriteLine("Apples: " + fruitBasket["Apples"]);
        
        // Uppdaterar ett värde
        fruitBasket["Apples"] = 7;
        Console.WriteLine("Uppdaterade Äpplen: " + fruitBasket["Apples"]);
        
        // Tar bort ett nyckel-värde-par
        fruitBasket.Remove("Oranges");

        // Itererar över ordboken
        foreach (var par in fruitBasket)
        {
            Console.WriteLine(par.Key + ": " + par.Value);
        }
    }
}
```
Exempelutdata:
```
Apples: 5
Uppdaterade Äpplen: 7
Apples: 7
```

Detta exempel visar hur man skapar en ordbok, lägger till, åtkomster, uppdaterar och tar bort element, samt itererar över den.

## Fördjupning

Konceptet med associativa fält går tillbaka till deras användning i skriptspråk som Perl och PHP, där de erbjuder flexibilitet i hanteringen av datasamlingar. I C# är `Dictionary<TKey, TValue>` den faktiska implementeringen, introducerad i .NET Framework 2.0. Den lagrar data i en hashtabell, vilket säkerställer effektiva sökningar, tillägg och borttagningar.

Det är dock värt att notera att även om ordböcker är otroligt mångsidiga, kanske de inte alltid är ditt bästa val. För att upprätthålla ordnade samlingar kan du titta på `SortedDictionary<TKey, TValue>` eller `SortedList<TKey, TValue>`, vilka erbjuder sorterad ordning till priset av långsammare insättningar och borttagningar. För scenarier som kräver trådsäkerhet lägger `ConcurrentDictionary<TKey, TValue>` till overhead men säkerställer säker åtkomst från flera trådar utan manuell låsning.

Slutligen beror valet av en implementering av associativa fält i C# på dina specifika behov med avseende på ordning, prestanda, och trådsäkerhet.
