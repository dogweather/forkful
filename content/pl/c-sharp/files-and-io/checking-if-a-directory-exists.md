---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:08.298215-07:00
description: "Jak to zrobi\u0107: C# zapewnia przestrze\u0144 nazw `System.IO`, kt\xF3\
  ra zawiera klas\u0119 `Directory`, oferuj\u0105c\u0105 bezpo\u015Bredni spos\xF3\
  b na sprawdzenie istnienia katalogu za\u2026"
lastmod: '2024-03-13T22:44:35.422745-06:00'
model: gpt-4-0125-preview
summary: "C# zapewnia przestrze\u0144 nazw `System.IO`, kt\xF3ra zawiera klas\u0119\
  \ `Directory`, oferuj\u0105c\u0105 bezpo\u015Bredni spos\xF3b na sprawdzenie istnienia\
  \ katalogu za pomoc\u0105 metody `Exists`."
title: Sprawdzanie, czy katalog istnieje
weight: 20
---

## Jak to zrobić:


### Używając System.IO
C# zapewnia przestrzeń nazw `System.IO`, która zawiera klasę `Directory`, oferującą bezpośredni sposób na sprawdzenie istnienia katalogu za pomocą metody `Exists`.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\ExampleDirectory";

        // Sprawdź, czy katalog istnieje
        bool directoryExists = Directory.Exists(directoryPath);

        // Wydrukuj wynik
        Console.WriteLine("Katalog istnieje: " + directoryExists);
    }
}
```

**Przykładowy wynik:**

```
Katalog istnieje: False
```

W przypadku, gdy katalog istnieje pod ścieżką `C:\ExampleDirectory`, wynik będzie `True`.

### Używając System.IO.Abstractions do testów jednostkowych
Gdy chodzi o możliwość testowania jednostkowego kodu, szczególnie gdy wchodzi on w interakcję z systemem plików, pakiet `System.IO.Abstractions` jest popularnym wyborem. Pozwala on abstrahować i udawać operacje na systemie plików w testach. Oto jak można sprawdzić istnienie katalogu, korzystając z tego podejścia:

Najpierw upewnij się, że zainstalowałeś pakiet:

```
Install-Package System.IO.Abstractions
```

Następnie możesz wstrzyknąć `IFileSystem` do swojej klasy i używać go do sprawdzania, czy katalog istnieje, co ułatwia testowanie jednostkowe.

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

        Console.WriteLine("Katalog istnieje: " + directoryExists);
    }
}
```

**Przykładowy wynik:**

```
Katalog istnieje: False
```

To podejście rozdziela logikę aplikacji od bezpośredniego dostępu do systemu plików, czyniąc kod bardziej modułowym, testowalnym i łatwym do utrzymania.
