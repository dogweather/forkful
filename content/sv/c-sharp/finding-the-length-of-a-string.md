---
title:    "C#: Hitta längden på en sträng"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en vanlig uppgift som du kommer att stöta på när du arbetar med C# -programmering. Det kan vara användbart när du behöver kontrollera om en användares inmatning är för lång eller när du behöver skapa en begränsning för antalet tecken som kan matas in i ett textfält.

## Hur man gör det

För att hitta längden på en sträng i C# använder du metoden "Length" som är tillgänglig för alla strängar. Du kan se en enkel kod som visar detta nedan:

```C#
string myString = "Hej, det här är en sträng!";
Console.WriteLine(myString.Length);
```

I detta exempel skapas en ny sträng med texten "Hej, det här är en sträng!" och sedan används "Length" metoden för att hitta dess längd. Output av koden skulle vara "26" eftersom det är antalet tecken i strängen.

En annan användbar metod för att hitta längden på en sträng är "Count". Denna metod fungerar på samma sätt som "Length" men kan också användas för att hitta antalet tecken eller ord i en sträng beroende på vad du behöver.

```C#
string myString = "Den här strängen har 8 ord.";
Console.WriteLine(myString.Count());
```

I detta exempel kommer "Count" metoden att returnera 8 eftersom det är antalet ord i strängen.

## Djupdykning

En viktig sak att komma ihåg när du arbetar med strängar är att längden inkluderar alla tecken, inklusive mellanslag och specialtecken. Om du vill hitta längden på en sträng exklusive mellanslag kan du använda metoden "Trim".

```C#
string myString = " 5 mellanslag.";
Console.WriteLine(myString.Length);
Console.WriteLine(myString.Trim().Length);
```

I detta exempel kommer output att vara "10" för den första utskriften eftersom mellanslaget räknas in. Men med hjälp av "Trim" metoden kommer den andra utskriften att vara "9" eftersom mellanslaget har tagits bort.

## Se också

- [C# Dokumentation om strängar](https://docs.microsoft.com/sv-se/dotnet/csharp/programming-guide/strings/)
- [Skillnad mellan Length och Count metoder](https://www.tutorialspoint.com/How-to-find-the-length-of-a-String-in-Csharp)