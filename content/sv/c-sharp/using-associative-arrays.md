---
title:                "Att använda associativa arrayer"
date:                  2024-01-30T19:10:16.994809-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda associativa arrayer"
programming_language: "C#"
category:             "C#"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
