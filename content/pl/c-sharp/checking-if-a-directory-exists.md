---
title:                "Sprawdzanie, czy katalog istnieje"
date:                  2024-01-19
simple_title:         "Sprawdzanie, czy katalog istnieje"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Sprawdzanie czy katalog istnieje to sposób na uniknięcie błędów, gdy aplikacja próbuje operować na plikach. Programiści to robią, aby upewnić się, że operacja na plikach nie zakończy się niepowodzeniem z powodu braku katalogu.

## How to (Jak to zrobić):
Sprawdźmy, jak łatwo możemy to zrobić w C#. Skorzystamy z klasy `Directory` z przestrzeni nazw `System.IO`.

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string path = @"C:\ExampleDirectory";

        if (Directory.Exists(path))
        {
            Console.WriteLine("Katalog istnieje!");
        }
        else
        {
            Console.WriteLine("Katalog nie istnieje.");
        }
    }
}
```
Odpalając ten kod zobaczysz:
```
Katalog istnieje!
```
lub
```
Katalog nie istnieje.
```
zależnie od tego, czy katalog istnieje czy nie.

## Deep Dive (Dogłębna analiza):
Sprawdzanie istnienia katalogu jest ważną czynnością w programowaniu już od wczesnych lat systemów plików. W C#, `Directory.Exists` jest standardowym podejściem i częścią .NET Framework od wersji 1.0. 

Alternatywą jest próba wykonania operacji na katalogu i obsłużenie ewentualnego wyjątku, ale to nie jest zalecane jako pierwsza linia obrony. Jest to tzw. programowanie przez wyjątki i może prowadzić do wolniejszego działania aplikacji w przypadku częstego nieistnienia katalogu.

Za kulisy, `Directory.Exists` wywołuje natywne funkcje systemu operacyjnego, aby stwierdzić status katalogu, co jest efektywne, ale wymaga również zrozumienia, że różne systemy operacyjne mogą zwracać różne wyniki (np. wrażliwość na wielkość liter w ścieżkach).

## See Also (Zobacz również):
- Dokumentacja Microsoft dla `Directory.Exists`: [Link](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- Microsoft .NET API Documentation: [Link](https://docs.microsoft.com/en-us/dotnet/)
- Artykuł o obsłudze wyjątków w C#: [Link](https://docs.microsoft.com/en-us/dotnet/standard/exceptions/)
