---
title:                "Odczytywanie pliku tekstowego"
date:                  2024-01-20T17:54:19.969488-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Czytanie pliku tekstowego to ładowanie jego zawartości do pamięci programu. Programiści robią to, by przetworzyć dane, wczytać konfiguracje, lub po prostu wyświetlić tekst użytkownikowi.

## How to: (Jak to zrobić:)
```C#
using System;
using System.IO;

class ReadTextFileExample
{
    static void Main()
    {
        string filePath = "example.txt";

        // Czytanie całego pliku na raz
        string fileContent = File.ReadAllText(filePath);
        Console.WriteLine("Zawartość pliku:");
        Console.WriteLine(fileContent);

        // Czytanie pliku linia po linii
        Console.WriteLine("\nLinie pliku:");
        string[] fileLines = File.ReadAllLines(filePath);
        foreach (string line in fileLines)
        {
            Console.WriteLine(line);
        }

        // Czytanie pliku z użyciem StreamReader
        Console.WriteLine("\nZawartość pliku ze StreamReader:");
        using (StreamReader reader = new StreamReader(filePath))
        {
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                Console.WriteLine(line);
            }
        }
    }
}
```

```plaintext
Zawartość pliku:
To jest treść pliku tekstowego.

Linie pliku:
To
jest
treść
pliku
tekstowego.

Zawartość pliku ze StreamReader:
To
jest
treść
pliku
tekstowego.
```

## Deep Dive (Głębsze spojrzenie)
Czytanie plików tekstowych w C# ma swoje korzenie w wcześniejszych językach i modelach IO. Klasa `System.IO.File` pojawiła się w .NET Framework 1.0 w 2002 roku i ewoluowała przez lata. Do alternatyw należą `File.ReadAllBytes` dla danych binarnych oraz `FileStream` dla zaawansowanego sterowania IO.

Implementacja zależy od potrzeb: `File.ReadAllText` i `File.ReadAllLines` są proste ale mogą być problematyczne przy dużych plikach ze względu na wykorzystanie pamięci. `StreamReader` czyta strumieniowo, co jest efektywne dla dużych plików.

## See Also (Zobacz także)
- Dokumentacja Microsoft o `System.IO.File`: [https://docs.microsoft.com/en-us/dotnet/api/system.io.file](https://docs.microsoft.com/en-us/dotnet/api/system.io.file)
- Dokumentacja Microsoft o `StreamReader`: [https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader)
- Przewodnik Microsoft po plikach i strumieniach we/wy: [https://docs.microsoft.com/en-us/dotnet/standard/io/](https://docs.microsoft.com/en-us/dotnet/standard/io/)
