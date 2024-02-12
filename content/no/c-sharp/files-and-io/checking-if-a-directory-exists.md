---
title:                "Sjekker om en mappe eksisterer"
aliases: - /no/c-sharp/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:18.966699-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sjekker om en mappe eksisterer"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & hvorfor?

Å sjekke om en mappe eksisterer i C# innebærer å verifisere tilstedeværelsen av en mappe på en spesifisert bane i filsystemet. Programmerere gjør dette for å unngå feil som å forsøke å lese fra eller skrive til en ikke-eksisterende mappe, noe som sikrer jevnere manipulasjoner med filer og mapper.

## Hvordan:

### Bruke System.IO

C# tilbyr `System.IO`-navnerommet som inneholder `Directory`-klassen, som tilbyr en direkte måte å sjekke for eksistensen av en mappe gjennom `Exists`-metoden.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\EksempelMappe";

        // Sjekk om mappen eksisterer
        bool directoryExists = Directory.Exists(directoryPath);

        // Skriv ut resultatet
        Console.WriteLine("Mappe eksisterer: " + directoryExists);
    }
}
```

**Eksempel på utskrift:**

```
Mappe eksisterer: False
```

I tilfelle mappen faktisk eksisterer på banen `C:\EksempelMappe`, vil utskriften være `True`.

### Bruke System.IO.Abstractions for enhetstesting

Når det kommer til å gjøre koden din enhetstestbar, spesielt når den samhandler med filsystemet, er pakken `System.IO.Abstractions` et populært valg. Den lar deg abstrahere og mocke filsystemoperasjoner i testene dine. Her er hvordan du kan sjekke for en mappe sin eksistens ved å bruke denne tilnærmingen:

Først, sørg for at du har installert pakken:

```
Install-Package System.IO.Abstractions
```

Deretter kan du injisere et `IFileSystem` i klassen din og bruke det til å sjekke om en mappe eksisterer, noe som tillater enklere enhetstesting.

```csharp
using System;
using System.IO.Abstractions;

class Program
{
    private readonly IFileSystem _fileSystem;

    public Program(IFileSystem fileSystem)
    {
        _fileSystem = fileSystem;
    }

    public bool CheckDirectoryExists(string directoryPath)
    {
        return _fileSystem.Directory.Exists(directoryPath);
    }

    static void Main()
    {
        var fileSystem = new FileSystem();
        var program = new Program(fileSystem);

        string directoryPath = @"C:\EksempelMappe";
        bool directoryExists = program.CheckDirectoryExists(directoryPath);

        Console.WriteLine("Mappe eksisterer: " + directoryExists);
    }
}
```

**Eksempel på utskrift:**

```
Mappe eksisterer: False
```

Denne tilnærmingen skiller applikasjonslogikken din fra direkte tilgang til filsystemet, noe som gjør koden din mer modulær, testbar og vedlikeholdbar.
