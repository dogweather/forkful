---
title:                "Kontrollera om en mapp existerar"
html_title:           "C#: Kontrollera om en mapp existerar"
simple_title:         "Kontrollera om en mapp existerar"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp existerar är en viktig del av filhantering inom programmering. Genom att ha denna funktion i ditt C#-program kan du hantera eventuella fel och undvika att försöka öppna en mapp som inte finns.

## Så här gör du

För att kontrollera om en mapp existerar i C# kan du använda dig av metoden `Directory.Exists()`. Denna metod tar in en sträng som representerar sökvägen till mappen som du vill kontrollera. Om mappen existerar kommer metoden att returnera `true`, annars `false`.

```C#
if(Directory.Exists(@"C:\Users\Example\Desktop"))
{
    Console.WriteLine("Mappen existerar.");
}
else
{
    Console.WriteLine("Mappen existerar inte.");
}

// Output: Mappen existerar.
```

Om du vill kontrollera en mapp inuti en annan mapp kan du använda dig av `Path.Combine()` för att kombinera sökvägarna.

```C#
string parentPath = @"C:\Users\Example\Desktop";
string childFolder = "NewFolder";

if(Directory.Exists(Path.Combine(parentPath, childFolder)))
{
    Console.WriteLine("Mappen existerar.");
}
else
{
    Console.WriteLine("Mappen existerar inte.");
}

// Output: Mappen existerar inte.
```

## Djupdykning

En sak att tänka på när du använder `Directory.Exists()` är att det endast kontrollerar om själva mappen existerar, och inte om det finns något innehåll i mappen. Om du vill kontrollera om en mapp är tom kan du använda dig av metoden `Directory.GetFileSystemEntries()` som returnerar en array av filer och mappar som finns i den angivna mappen.

Det är också viktigt att komma ihåg att `Directory.Exists()` inte garanterar att mappen kommer att finnas kvar när du försöker öppna den. Om du behöver göra något med en mapp efter att ha kontrollerat om den existerar kan det vara en god idé att lägga till felhantering för att undvika eventuella problem.

## Se även

- [Microsoft's dokumentation om Directory.Exists()](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- [C# Coding Conventions (C# Programming Guide)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/inside-a-program/coding-conventions)
- [Path.Combine() - C# Guide](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.combine?view=net-5.0)