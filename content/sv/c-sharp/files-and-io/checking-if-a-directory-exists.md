---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:11.740296-07:00
description: "Att kontrollera om en katalog finns i C# inneb\xE4r att man verifierar\
  \ n\xE4rvaron av en mapp p\xE5 en angiven s\xF6kv\xE4g i filsystemet. Programmerare\
  \ g\xF6r detta f\xF6r\u2026"
lastmod: '2024-02-25T18:49:36.226259-07:00'
model: gpt-4-0125-preview
summary: "Att kontrollera om en katalog finns i C# inneb\xE4r att man verifierar n\xE4\
  rvaron av en mapp p\xE5 en angiven s\xF6kv\xE4g i filsystemet. Programmerare g\xF6\
  r detta f\xF6r\u2026"
title: Kontrollera om en katalog existerar
---

{{< edit_this_page >}}

## Vad & Varför?

Att kontrollera om en katalog finns i C# innebär att man verifierar närvaron av en mapp på en angiven sökväg i filsystemet. Programmerare gör detta för att undvika fel såsom att försöka läsa från eller skriva till en icke-existerande katalog, vilket säkerställer smidigare hantering av filer och kataloger.

## Hur man gör:

### Använda System.IO

C# tillhandahåller namnrymden `System.IO` som innehåller `Directory`-klassen, som erbjuder ett direkt sätt att kontrollera en katalogs existens genom metoden `Exists`.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\ExampleDirectory";

        // Kontrollera om katalogen finns
        bool directoryExists = Directory.Exists(directoryPath);

        // Skriv ut resultatet
        Console.WriteLine("Directory exists: " + directoryExists);
    }
}
```

**Exempel på utdata:**

```
Directory exists: False
```

I fall katalogen faktiskt finns på sökvägen `C:\ExampleDirectory`, kommer utdatan att vara `True`.

### Använda System.IO.Abstractions för enhetstestning

När det kommer till att göra din kod enhetstestbar, speciellt när den interagerar med filsystemet, är paketet `System.IO.Abstractions` ett populärt val. Det låter dig abstrahera och förlöjliga (mocka) filsystemoperationer i dina tester. Så här kan du kontrollera om en katalog finns genom att använda detta tillvägagångssätt:

Först, se till att du har installerat paketet:

```
Install-Package System.IO.Abstractions
```

Sedan kan du injicera ett `IFileSystem` i din klass och använda det för att kontrollera om en katalog finns, vilket underlättar enhetstestning.

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

        string directoryPath = @"C:\ExampleDirectory";
        bool directoryExists = program.CheckDirectoryExists(directoryPath);

        Console.WriteLine("Directory exists: " + directoryExists);
    }
}
```

**Exempel på utdata:**

```
Directory exists: False
```

Detta tillvägagångssätt separerar din applikationslogik från direkt åtkomst till filsystemet, vilket gör din kod mer modulär, testbar och underhållbar.
