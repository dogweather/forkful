---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:08.298215-07:00
description: "Sprawdzanie, czy katalog istnieje w C#, polega na weryfikowaniu obecno\u015B\
  ci folderu w okre\u015Blonej \u015Bcie\u017Cce w systemie plik\xF3w. Programi\u015B\
  ci robi\u0105 to, aby\u2026"
lastmod: '2024-03-13T22:44:35.422745-06:00'
model: gpt-4-0125-preview
summary: "Sprawdzanie, czy katalog istnieje w C#, polega na weryfikowaniu obecno\u015B\
  ci folderu w okre\u015Blonej \u015Bcie\u017Cce w systemie plik\xF3w. Programi\u015B\
  ci robi\u0105 to, aby\u2026"
title: Sprawdzanie, czy katalog istnieje
weight: 20
---

## Co i dlaczego?

Sprawdzanie, czy katalog istnieje w C#, polega na weryfikowaniu obecności folderu w określonej ścieżce w systemie plików. Programiści robią to, aby uniknąć błędów, takich jak próby odczytu z lub zapisu do nieistniejącego katalogu, zapewniając płynniejsze manipulacje plikami i katalogami.

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
