---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "C#: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Sprawdzanie, czy katalog istnieje, to proces umożliwiający określenie, czy dany katalog jest present na dysku. Programiści wykonują to sprawdzanie, aby uniknąć błędów podczas operacji na plikach lub katalogach, które mogą nie istnieć.

## Jak to zrobić:

Aby sprawdzić, czy katalog istnieje w C#, użyjemy klasy `Directory` z przestrzeni nazw `System.IO`. Poniżej znajduje się przykładowy kod:

```C#
using System.IO;

class Program
{
    static void Main()
    {
        if(Directory.Exists("C:\\MyDirectory"))
        {
            System.Console.WriteLine("Katalog istnieje");
        }
        else
        {
            System.Console.WriteLine("Katalog nie istnieje");
        }
    }
}
```

Wynik wyjścia:
```
Katalog istnieje
```
lub
```
Katalog nie istnieje
```

## W Głębi Tematu

Sprawdzanie, czy katalog istnieje, to podstawowy element wielu aplikacji, który umożliwia bezpieczne operacje na plikach. Wynika to z historii systemów plików, które zawsze miały możliwość niewłaściwego odwołania się do niewłaściwego pliku lub katalogu.

W C# istnieje kilka alternatyw dla metody `Directory.Exists()`, takich jak `File.Exists()` dla sprawdzania plików lub `DirectoryInfo.Exists`, która jest częścią obiektu DirectoryInfo.

Kiedy używasz `Directory.Exists()`, metoda ta stara się zlokalizować katalog na fizycznym dysku za pomocą dostarczonej ścieżki. Jeśli katalogu nie ma, metoda zwraca false.

## Zobacz Również

Możesz dowiedzieć się więcej na temat operacji na plikach i katalogach w C#, odwiedzając następujące linki:

- [Dokumentacja Microsoft na temat klasy Directory](https://docs.microsoft.com/pl-pl/dotnet/api/system.io.directory?view=net-5.0)
- [StackOverflow: Sprawdzanie, czy katalog istnieje w C#](https://stackoverflow.com/questions/1395205/better-way-to-check-if-a-path-is-a-file-or-a-directory)
- [MSDN: Jak: Sprawdzić, czy plik lub katalog istnieje](https://docs.microsoft.com/pl-pl/dotnet/standard/io/how-to-check-if-a-file-or-directory-exists)