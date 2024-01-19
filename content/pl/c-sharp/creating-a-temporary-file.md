---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?
Tworzenie tymczasowych plików polega na generowaniu plików o krótkotrwałym okresie istnienia, do przechowywania tymczasowych danych. Programiści robią to, aby zdobyć miejsce na dysku do krótkotrwałego przechowywania danych, zazwyczaj jak wynik operacji, które nie mogą być przechowane w pamięci.

## Jak to zrobić:
```C#
using System.IO;

namespace TemporaryFileExample
{
  class Program
  {
    static void Main(string[] args)
    {
    string tempFile = Path.GetTempFileName();

    using (StreamWriter sw = new StreamWriter(tempFile))
    {
      sw.WriteLine("Tak wygląda tymczasowy plik!");
    }

    using (StreamReader sr = new StreamReader(tempFile))
    {
      Console.WriteLine(sr.ReadToEnd());
    }

    File.Delete(tempFile);  // usuń plik po użyciu
    }
  }
}
```
Po uruchomieniu powyższego kodu, otrzymamy wynik:
```
Tak wygląda tymczasowy plik!
```

## Pogłębiona analiza
Tworzenie tymczasowych plików to koncepcja, która posiada swoją korzeń w czasach, gdy pamięci RAM było mało i operacje wymagały dodatkowego miejsca do przechowywania danych. Teraz, mimo że mamy więcej pamięci RAM, nadal przydaje się to do zapewnienia niezawodności i zabezpieczenia. 

Jeśli chodzi o alternatywy, można wykorzystać bazy danych lub pamięć podręczną, ale wymaga to odpowiednich zasobów i konieczności zarządzania nimi. 

O szczegółach implementacji: `Path.GetTempFileName()` tworzy plik o unikalnym nazwie w folderze tymczasowym systemu, zabezpieczając nas przed konfliktami nazw.

## Zobacz także
- Dokumentacja `GetTempFileName()`: https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename
- Więcej o plikach tymczasowych: https://en.wikipedia.org/wiki/Temporary_folder
- Poradnik o zarządzaniu plikami w C#: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-create-a-file-or-folder