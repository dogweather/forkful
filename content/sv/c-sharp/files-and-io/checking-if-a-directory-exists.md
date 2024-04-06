---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:11.740296-07:00
description: "Hur man g\xF6r: C# tillhandah\xE5ller namnrymden `System.IO` som inneh\xE5\
  ller `Directory`-klassen, som erbjuder ett direkt s\xE4tt att kontrollera en katalogs\u2026"
lastmod: '2024-03-13T22:44:37.927812-06:00'
model: gpt-4-0125-preview
summary: "C# tillhandah\xE5ller namnrymden `System.IO` som inneh\xE5ller `Directory`-klassen,\
  \ som erbjuder ett direkt s\xE4tt att kontrollera en katalogs existens genom metoden\
  \ `Exists`."
title: Kontrollera om en katalog existerar
weight: 20
---

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
