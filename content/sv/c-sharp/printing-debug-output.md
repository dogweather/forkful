---
title:    "C#: Utskrift av felmeddelanden"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Felsökningsutmatning är en viktig del av programmering. Det hjälper dig att identifiera och åtgärda problem i din kod på ett snabbt och effektivt sätt. Genom att skriva ut olika värden och variabler i din kod kan du se hur de ändras och spåra eventuella fel.

## Hur man gör

Att skriva ut felsökningsinformation är enkelt och kan göras på olika sätt. Här är några exempel på hur du kan skriva ut felsökningsutmatning i C#:

```C#
int number = 42;
string name = "John";

Console.WriteLine(number); // Skriver ut värdet av variabeln "number"
Console.WriteLine("Hej " + name); // Skriver ut text plus värdet av variabeln "name"

// Resultat:
// 42
// Hej John
```

Du kan också använda metoden `Debug.WriteLine()` för att skriva ut felsökningsinformation. Detta kan vara användbart när du behöver felsöka i en större applikation och vill ha möjlighet att kontrollera utmatningen vid ett senare tillfälle.

```C#
int x = 5;
int y = 10;
Debug.WriteLine($"Summan av {x} och {y} är {x+y}"); // Skriver ut en sträng med variabler
```

För att skriva ut information om en specifik variabel eller ett objekt, kan du använda `Debug.Print()`-metoden.

```C#
Person person = new Person("Anna", 26, "Stockholm");
Debug.Print(person.Name); // Skriver ut värdet av "Name"-egenskapen i objektet
```

Felsökningsutmatning kan också vara till nytta när du vill se vad som händer i en loop eller en funktion. Du kan enkelt skriva ut information före, under och efter en loop, eller vid olika händelser i en funktion.

## Djupdykning

Det finns flera olika alternativ för felsökningsutmatning i C#, såsom `Console.WriteLine()`, `Debug.WriteLine()` och `Debug.Print()`. Men det går också att göra mer avancerade utskrifter som kan göra ditt felsökningsarbete ännu enklare.

En sådan möjlighet är att använda `string.Format()`-metoden för att formatera felsökningsutmatning på ett snyggare sätt.

```C#
// En funktion som dubblar ett tal och skriver ut det
int DoubleNumber(int number)
{
    int result = number * 2;
    string message = string.Format("Det dubbla av {0} är {1}", number, result);
    Console.WriteLine(message);
    
    return result;
}

int result = DoubleNumber(7); // Anrop av funktionen ovan

// Resultat:
// Det dubbla av 7 är 14
```

Du kan också använda `string interpolation` för att formatera felsökningsutmatning på ett enklare sätt.

```C#
// Samma funktion som ovan, fast med string interpolation
int DoubleNumber(int number)
{
    int result = number * 2;
    string message = $"Det dubbla av {number} är {result}";
    Console.WriteLine(message);
    
    return result;
}

int result = DoubleNumber(7); // Anrop av funktionen ovan

// Resultat:
// Det dubbla av 7 är 14
```

Det finns också möjlighet att använda debuggervyn i Visual Studio för att se felsökningsutmatning i realtid. Detta kan vara särskilt användbart när du behöver snabbt hitta ett problem i din kod.

## Se även

- [Microsoft dokumentation om felsökningsutmatning i C#](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-write-to-the-debug-output)

- [En guide till felsökningsutmatning i C#](https://www.tutorialspoint.com/csharp/csharp_debugging.htm)

- [Video tutorial om felsökningsutmatning i C#](https://www.youtube.com/watch?v=GnyTNyOSitg)