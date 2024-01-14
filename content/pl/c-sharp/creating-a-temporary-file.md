---
title:                "C#: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych jest nieodzownym elementem procesu tworzenia aplikacji w języku C#. Pliki tymczasowe są przydatne w wielu przypadkach, na przykład do przechowywania danych tymczasowych, załączników lub do obsługi plików wykonywalnych. W tym artykule dowiesz się, jak w prosty sposób tworzyć i zarządzać plikami tymczasowymi w swoim kodzie C#.

## Jak to zrobić

Tworzenie plików tymczasowych w języku C# jest bardzo proste i wymaga zaledwie kilku linii kodu. Pierwszym krokiem jest utworzenie obiektu klasy `StreamWriter`, który umożliwi zapisywanie danych do pliku. Następnie wykorzystujemy funkcję `GetTempFileName()` z przestrzeni nazw `Path`, aby wygenerować unikalną nazwę dla naszego pliku tymczasowego. Na koniec, wystarczy użyć funkcji `Write()` lub `WriteLine()` obiektu `StreamWriter` do zapisania danych do pliku.

```C#
// Utworzenie obiektu StreamWriter zapisującego do naszego pliku tymczasowego
StreamWriter sw = new StreamWriter(Path.GetTempFileName());

// Wygenerowanie unikalnej nazwy dla pliku tymczasowego i wyświetlenie jej w konsoli
string tempFile = Path.GetTempFileName();
Console.WriteLine("Wygenerowana nazwa dla pliku tymczasowego: " + tempFile);

// Zapisywanie danych do pliku tymczasowego
sw.WriteLine("Przykładowe dane tymczasowe");
sw.WriteLine("Możesz dodać dowolne dane do pliku");
sw.WriteLine("I zapisując je, możesz je później odczytać");

// Zamknięcie obiektu StreamWriter i zapisanie zmian w pliku tymczasowym
sw.Close();
```

Po wykonaniu powyższego kodu, w katalogu tymczasowym na Twoim komputerze pojawi się nowy plik o unikalnej nazwie. W tym przypadku nazwa ta to `tmp7AE8.tmp`. Możesz również wybrać inną lokalizację i nazwę dla swojego pliku tymczasowego, zmieniając parametry funkcji `GetTempFileName()`.

## Deep Dive

Jedną z najważniejszych korzyści tworzenia plików tymczasowych jest automatyczna ich usuwania po zakończeniu programu, dzięki funkcji `GetTempFileName()`. Jest to idealne rozwiązanie dla aplikacji, które muszą używać tymczasowych plików, ale nie chcą zaśmiecać systemu. Pamiętaj również, że pliki tymczasowe mogą również zawierać dane o różnych typach, takich jak tekst, obrazy czy nawet pliki wykonywalne.

Ponadto, możesz również wykorzystać funkcje z przestrzeni nazw `Path` do zarządzania swoimi plikami tymczasowymi. Na przykład, możesz sprawdzić, czy dany plik tymczasowy istnieje, używając funkcji `File.Exists(path)`, lub usunąć plik za pomocą funkcji `File.Delete(path)`.

## Zobacz również

* [Dokumentacja Microsoft na temat tworzenia plików tymczasowych](https://docs.microsoft.com/pl-pl/dotnet/standard/io/how-to-create-temporary-files) 
* [Tutorial na temat zarządzania plikami w C#](https://www.tutorialspoint.com/csharp/csharp_file_create.htm)
* [Przykładowy projekt na GitHubie wykorzystujący pliki tymczasowe w C#](https://github.com/grady212/tempFileManager)