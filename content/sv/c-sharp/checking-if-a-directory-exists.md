---
title:    "C#: Kontrollera om en mapp finns"
keywords: ["C#"]
---

{{< edit_this_page >}}

Varför: Att kontrollera om en mapp finns kan vara en viktig del av programutveckling för att säkerställa att all nödvändig filhantering görs på rätt plats.

Hur man gör det: För att kontrollera om en mapp finns i C # kan vi använda metoden "Directory.Exists ()". Här är ett exempel:

````C#
if (Directory.Exists("C:\\Users\\Public\\Documents"))
{
    Console.WriteLine("Mappen finns.");
}
else
{
    Console.WriteLine("Mappen finns inte.");
}
````

Output: Om mappen finns kommer meddelandet "Mappen finns." att skrivas ut, annars skrivs "Mappen finns inte." ut. Detta kan vara användbart för att utföra olika åtgärder beroende på om mappen faktiskt finns eller inte.

Deep Dive: När vi använder metoden "Directory.Exists ()" är det viktigt att notera att den returnerar en boolesk värde - true om mappen finns och false om den inte gör det. Det är också viktigt att se till att ange den fullständiga sökvägen till mappen för att metoden ska fungera korrekt.

Se också:
- [Microsoft dokumentation om Directory.Exists ()](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=net-5.0)
- [C # Guide: Fil- och mapphantering](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/)
- [Kontrollera om en fil eller mapp finns med C #](https://www.c-sharpcorner.com/article/check-if-a-file-or-directory-exists-in-C-Sharp/)

Se: Kontrollera om den angivna sökvägen leder till en mapp eller en fil