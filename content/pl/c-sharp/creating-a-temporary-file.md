---
title:    "C#: Tworzenie pliku tymczasowego"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych jest istotnym elementem wielu aplikacji. Mogą one być przydatne do przechowywania tymczasowych danych lub do dzielenia się plikami z innymi użytkownikami. W tym blogowym wpisie dowiesz się, jak stworzyć plik tymczasowy w języku C#.

## Jak to zrobić

Poniżej znajdują się przykładowy kod w języku C#, który pozwoli Ci utworzyć i zapisać plik tymczasowy na Twoim komputerze. W każdym kroku jest również wypisany wynik, abyś mógł zobaczyć, jak wyglądają stworzone pliki.

```C#
// Importowanie niezbędnych bibliotek
using System;
using System.IO;

// Utworzenie nazwy pliku tymczasowego
string tempFileName = Path.GetTempFileName();

// Wypisanie nazwy pliku na konsoli
Console.WriteLine("Stworzony plik tymczasowy: " + tempFileName);

// Zapisanie przykładowych danych do pliku
using (StreamWriter sw = File.CreateText(tempFileName))
{
    sw.WriteLine("To jest przykładowy tekst w pliku tymczasowym.");
    sw.WriteLine("Możesz dodać więcej danych w ten sam sposób.");
}

// Odczytanie danych z pliku
string[] lines = File.ReadAllLines(tempFileName);
Console.WriteLine("Zawartość pliku:");
foreach (string line in lines)
{
    Console.WriteLine(line);
}

// Zmiana nazwy pliku
string newFileName = Path.GetTempFileName();
Console.WriteLine("Zmieniona nazwa pliku tymczasowego: " + newFileName);
```
Sample output:

Stworzony plik tymczasowy: C:\Users\Username\AppData\Local\Temp\tmp1234.tmp
Zawartość pliku:
To jest przykładowy tekst w pliku tymczasowym.
Możesz dodać więcej danych w ten sam sposób.
Zmieniona nazwa pliku tymczasowego: C:\Users\Username\AppData\Local\Temp\tmp5678.tmp

## Deep Dive

Tworzenie plików tymczasowych jest możliwe dzięki wykorzystaniu klasy Path i jej metody GetTempFileName(). Ta metoda generuje unikalną nazwę pliku w folderze tymczasowym systemu operacyjnego. Następnie można wykorzystać klasę StreamWriter do zapisywania danych w pliku, a klasę File do odczytania tych danych. Istnieje również możliwość zmiany nazwy pliku tymczasowego na inną unikalną przy użyciu również metody GetTempFileName().

Innym sposobem na tworzenie plików tymczasowych jest wykorzystanie klasy Path i metody GetTempPath(), która zwraca ścieżkę do folderu tymczasowego systemu operacyjnego. Następnie możesz samodzielnie utworzyć nazwę pliku i wykorzystać ją w funkcjach StreamWriter i File. Jednak w takim przypadku musisz zagwarantować, że wybrana nazwa jest unikalna, aby uniknąć konfliktów z innymi plikami tymczasowymi.

## Zobacz również
- Dokumentacja Path.GetTempFileName() (https://docs.microsoft.com/pl-pl/dotnet/api/system.io.path.gettempfilename)
- Dokumentacja StreamWriter (https://docs.microsoft.com/pl-pl/dotnet/api/system.io.streamwriter)
- Dokumentacja File (https://docs.microsoft.com/pl-pl/dotnet/api/system.io.file)