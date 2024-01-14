---
title:    "C#: Tworzenie tymczasowego pliku"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego?

Stworzenie pliku tymczasowego może być niezbędne w niektórych kodach programów, szczególnie w przypadku manipulowania dużymi danymi lub w celu zapewnienia bezpieczeństwa. Pliki tymczasowe są również przydatne podczas testowania kodu i debugowania problemów.

## Jak to zrobić?

```C#
using System;
using System.IO;

namespace TemporaryFileExample
{
    class Program
    {
        static void Main(string[] args)
        {
            // Stwórz ścieżkę dla pliku tymczasowego
            string path = Path.GetTempFileName();

            // Zapisz w pliku
            using (FileStream fs = File.Create(path))
            {
                // Utwórz zmienną do zapisania w pliku
                Byte[] info = new UTF8Encoding(true).GetBytes("To jest przykładowy tekst zapisany w pliku tymczasowym.");

                // Zapisz informacje do pliku
                fs.Write(info, 0, info.Length);
            }

            // Odczytaj plik
            string readText = File.ReadAllText(path);

            // Wyświetl zawartość pliku
            Console.WriteLine(readText);

            // Pamiętaj o usunięciu pliku tymczasowego
            File.Delete(path);
        }
    }
}
```

**Wyjście:** To jest przykładowy tekst zapisany w pliku tymczasowym.

## Głębsze wgląd

Aby lepiej zrozumieć proces tworzenia pliku tymczasowego, warto wiedzieć, że plik ten jest tworzony w specjalnym folderze systemowym, w którym przechowywane są tymczasowe dane programów. Aby uzyskać dostęp do tego folderu, można użyć metody `GetTempPath()` z klasy `Path`. Plik tymczasowy jest również automatycznie usuwany po zamknięciu programu, dlatego nie musimy się martwić o jego czyszczenie.

## Zobacz również

- [Dokumentacja Microsoft o tworzeniu plików tymczasowych w C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.io.path.gettempfilename)
- [Porównanie plików tymczasowych i buforów w C#](https://www.c-sharpcorner.com/article/temporary-files-vs-temporary-buffer-in-C-Sharp/)