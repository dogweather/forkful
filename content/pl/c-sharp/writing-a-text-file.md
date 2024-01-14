---
title:                "C#: Tworzenie pliku tekstowego"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Dlaczego pisać pliki tekstowe w C#

Pisanie plików tekstowych w języku C# może być bardzo przydatnym narzędziem w pracy programisty. Pliki tekstowe pozwalają na przechowywanie i przetwarzanie danych w czytelnej dla człowieka formie. Mogą one być wykorzystane w różnych zastosowaniach, na przykład do tworzenia raportów, logów, konfiguracji aplikacji czy też prostych baz danych.

# Jak to zrobić

Aby napisać plik tekstowy w języku C#, należy najpierw zaimportować przestrzeń nazw System.IO. Następnie można wykorzystać klasę StreamWriter w celu zapisania danych do pliku. Przykładowy kod może wyglądać następująco:

```C#
using System.IO;

// Tworzenie obiektu StreamWriter i pliku "moj_plik.txt"
StreamWriter sw = new StreamWriter("moj_plik.txt");

// Zapisanie tekstu do pliku
sw.WriteLine("To jest przykładowy tekst");

// Zamknięcie pliku
sw.Close();
```

Po uruchomieniu powyższego kodu, w bieżącym folderze pojawi się plik "moj_plik.txt" zawierający wpisany przez nas tekst.

# Głębsze spojrzenie

Podczas tworzenia pliku tekstowego w C# można wykorzystać wiele różnych metod i klas, takich jak File, FileStream czy też Path. Warto również pamiętać o sposobie zapisu danych w pliku (np. kodowania znaków), aby uniknąć problemów z odczytem tych danych w przyszłości.

# Zobacz również

- Dokumentacja języka C# na temat zapisu plików tekstowych: https://docs.microsoft.com/pl-pl/dotnet/api/system.io.streamwriter
- Przykłady wykorzystania plików tekstowych w celach programistycznych: https://www.c-sharpcorner.com/uploadfile/8911c4/file-handling-in-C-Sharp/