---
title:                "Tworzenie pliku tymczasowego."
html_title:           "C#: Tworzenie pliku tymczasowego."
simple_title:         "Tworzenie pliku tymczasowego."
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie pliku tymczasowego jest częstym zadaniem w programowaniu, ponieważ pozwala na tymczasowe przechowywanie danych lub informacji. Jest to szczególnie przydatne w przypadku, gdy musimy przetworzyć lub utworzyć duże pliki, aby uniknąć obciążenia pamięci lub utraty danych.

## Jak to zrobić

Aby utworzyć plik tymczasowy w C#, należy użyć klasy Path i metody GetTempFileName(). Poniżej przedstawiono przykładowy kod:

```C#
string tempFilePath = Path.GetTempFileName();
```

Ten kod utworzy plik tymczasowy w folderze tymczasowym systemu operacyjnego. Można również podać własną nazwę pliku oraz ścieżkę, używając klasy File i metody Create. Przykładowy kod:

```C#
string myTempFile = Path.Combine(Path.GetTempPath(), "sample.txt"); 
// ustawiamy ścieżkę i nazwę pliku tymczasowego
File.Create(myTempFile); // tworzymy plik tymczasowy
```

Aby odwołać się do istniejącego pliku tymczasowego, po prostu użyj nazwy i ścieżki, w której został on utworzony.

## Deep Dive

W systemie operacyjnym plik tymczasowy jest przechowywany w folderze tymczasowym, który znajduje się w ścieżce określonej przez zmienną systemową TEMP lub TMP. Jest to zazwyczaj folder "AppData\Local\Temp" dla systemów Windows lub "/var/tmp" dla systemów Linux.

Warto również pamiętać, że plik tymczasowy jest usuwany automatycznie po zamknięciu programu, w którym został utworzony, lub po wywołaniu metody Delete(). Można także ustawić opcję, aby plik nie został usunięty po zamknięciu programu, co jest przydatne, gdy chcemy wykorzystać plik tymczasowy w kolejnych sesjach.

## Zobacz także

- [Dokumentacja Microsoft na temat klasy Path](https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=net-5.0)
- [Przykłady użycia klasy File](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-write-text-to-a-file)
- [Porady na temat wykorzystania plików tymczasowych w programowaniu](https://www.codeproject.com/Articles/122528/Temporary-Files-and-Folders-in-NET-3-5)