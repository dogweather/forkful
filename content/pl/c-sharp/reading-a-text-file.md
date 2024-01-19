---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Czytanie pliku tekstowego to proces odczytania danych zapisanych w formacie tekstowym. Programiści robią to, aby przetworzyć i manipulować danymi do różnych celów, takich jak przekształcanie tych danych w przydatne informacje.

## Jak to zrobić:
Biblioteka `System.IO` w C# zawiera wiele przydatnych funkcji do czytania plików tekstowych. Najprostszym sposobem jest użycie `File.ReadAllText()`.

```C#
using System.IO;

string path = @"C:\SomeDirectory\SomeFile.txt";
string readText = File.ReadAllText(path);
Console.WriteLine(readText);
```
Wyjście programu to zawartość pliku `SomeFile.txt`.

Aby odczytać plik wiersz po wierszu, można użyć `File.ReadLines()`.

```C#
foreach (string line in File.ReadLines(path))
{
    Console.WriteLine(line);
}
```
Wyjście programu to linie tekstu w `SomeFile.txt`.

## Głębsze zrozumienie
Czytanie plików tekstowych jest ważnym elementem wielu programów, ciągle się rozwija i ma długą historię, nie tylko w C#. Alternatywnie do wykorzystania klas `File`, istnieje możliwość użycia `StreamReader`, który oferuje większą kontrolę i bardziej zaawansowane funkcje.

```C#
using (StreamReader sr = new StreamReader(path))
{
    string line;
    while ((line = sr.ReadLine()) != null)
    {
        Console.WriteLine(line);
    }
}
```
To podejście jest bardziej rozbudowane, ale przydaje się w bardziej skomplikowanych sytuacjach.

## Zobacz też
Dla bardziej zaawansowanych operacji we/wy warto zainteresować się `System.IO.StreamReader` i `System.IO.StreamWriter`:
* Dokumentacja Microsoft .NET: https://docs.microsoft.com/pl-pl/dotnet/api/system.io.streamreader
* Dokumentacja Microsoft .NET: https://docs.microsoft.com/pl-pl/dotnet/api/system.io.streamwriter 

Dla operacji na plikach i katalogach:
* Dokumentacja Microsoft .NET: https://docs.microsoft.com/pl-pl/dotnet/api/system.io.file
* Dokumentacja Microsoft .NET: https://docs.microsoft.com/pl-pl/dotnet/api/system.io.directory