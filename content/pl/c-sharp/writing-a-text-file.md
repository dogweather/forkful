---
title:                "C#: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest nieodłączną częścią pisania programów w C#. Pliki tekstowe pozwalają nam na przechowywanie różnego rodzaju informacji w czytelnej dla człowieka formie, co ułatwia nam pracę z nimi w przyszłości. W poniższym artykule dowiesz się, w jaki sposób napisać plik tekstowy w języku C#.

## Jak to zrobić

Aby napisać plik tekstowy w C#, musimy wykonać kilka prostych kroków:

1. Najpierw musimy zaimportować przestrzeń nazw ```System.IO```, która zawiera klasy i metody umożliwiające manipulowanie plikami w C#.
2. Następnie tworzymy obiekt typu ```StreamWriter``` i przekazujemy mu ścieżkę do pliku, który chcemy utworzyć lub edytować.
3. W tym momencie możemy już używać różnych metod dostępnych w klasie ```StreamWriter```, takich jak ```WriteLine()``` czy ```Write()```, aby zapisywać dane do pliku.
4. Po zakończeniu zapisywania musimy zamknąć plik, wywołując metodę ```Close()``` na obiekcie ```StreamWriter```.
5. Gotowe! Nasz plik tekstowy został utworzony lub zaktualizowany zgodnie z naszymi oczekiwaniami.

```C#
using System.IO; // Importowanie przestrzeni nazw System.IO

// Tworzenie obiektu StreamWriter i przekazanie ścieżki do pliku
StreamWriter writer = new StreamWriter("moj_plik.txt");

// Zapisywanie danych do pliku
writer.WriteLine("To jest pierwsza linijka tekstu.");
writer.Write("A to jest druga linijka tekstu.");

// Zamykanie pliku
writer.Close();
```

Po uruchomieniu powyższego kodu, w naszym folderze znajdziemy plik o nazwie "moj_plik.txt" zawierający wprowadzone przez nas dane.

## Zanurzenie się w temat

Pisanie plików tekstowych może się wydawać prostym zadaniem, ale warto poznać kilka dodatkowych informacji na temat tego procesu.

### Tworzenie plików

Jeśli chcemy utworzyć nowy plik tekstowy, musimy wykorzystać obiekt typu ```StreamWriter``` w połączeniu z metodą ```CreateText()``` znajdującą się w klasie ```File```.

```C#
StreamWriter writer = File.CreateText("nowy_plik.txt");
```

### Obsługa wyjątków

Podczas manipulowania plikami tekstowymi warto zabezpieczyć się przed ewentualnymi błędami. Należy pamiętać o obsłudze wyjątków, takich jak np. ```IOException```, która może pojawić się w przypadku problemów z dostępem do pliku.

### Zapisywanie danych binarnych

Klasy i metody związane z obsługą plików tekstowych w C# służą głównie do zapisywania tekstu. Jeśli chcemy zapisać dane binarne, np. obrazy, musimy wykorzystać inne mechanizmy.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej na temat manipulowania plikami w C#, polecamy zapoznać się z poniższymi linkami:

- [Dokumentacja Microsoft na temat klasy StreamWriter](https://docs.microsoft.com/pl-pl/dotnet/api/system.io.streamwriter)
- [Poradnik dotyczący pracy z plikami w C#](https://www.c-sharpcorner.com/uploadfile/mahesh/working-with-streaming-in-C-Sharp/)
- [Przewodnik dla początkujących na temat plików w C#](https://www.tutorialspoint.com/csharp/csharp_files_io.htm)

Dziękujemy za przeczytanie naszego artykułu na temat pisania plików tekstowych w C#. M