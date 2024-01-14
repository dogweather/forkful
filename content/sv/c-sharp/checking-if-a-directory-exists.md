---
title:    "C#: Kontrollera om en mapp finns"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en katalog existerar kan vara en viktig del av programmering i C#. Det kan hjälpa till att säkerställa att din kod fungerar korrekt och undvika eventuella felmeddelanden. Det är också en bra praxis för att hantera filhantering i dina applikationer.

## Så här gör du

För att kontrollera om en katalog existerar i C#, kan du använda metoden `Directory.Exists()`. Detta är en inbyggd metod som tar en sträng som argument och returnerar en boolesk värde som indikerar om en katalog existerar eller inte. Låt oss titta på ett exempel:

```C#
if(Directory.Exists("C:\\Users\\Example\\Documents"))
{
    Console.WriteLine("Katalogen existerar.");
}
else
{
    Console.WriteLine("Katalogen existerar inte.");
}
```

I detta exempel använder vi metoden `Directory.Exists()` för att kontrollera om katalogen "C:\Users\Example\Documents" existerar. Om den gör det skriver vi ut ett meddelande som bekräftar att katalogen existerar, annars skriver vi ut att den inte existerar. Om du vill kontrollera en relativ sökväg istället för en absolut sökväg, kan du använda `Path.Combine()` för att kombinera en befintlig sökväg med den sökväg du vill kontrollera.

## Djupdykning

Metoden `Directory.Exists()` använder sig av objektet `FileSystemInfo` för att kontrollera om en katalog existerar. Detta objekt har en egenskap som heter `Exists` som returnerar sant om filen eller katalogen existerar. `Directory.Exists()` tar bara emot en sökväg som argument, men om du vill ha mer kontroll kan du använda `FileSystemInfo`-objektet direkt. Detta kan vara användbart om du vill använda mer avancerade sökningar eller filtreringar på dina filer och kataloger.

## Se även

- [Microsoft Docs: Directory.Exists()](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=net-5.0)
- [Microsoft Docs: FileSystemInfo.Exists](https://docs.microsoft.com/en-us/dotnet/api/system.io.filesysteminfo.exists?view=net-5.0)
- [C# How to Check if a Directory Exists](https://www.c-sharpcorner.com/article/c-sharp-how-to-check-if-directory-exists/)