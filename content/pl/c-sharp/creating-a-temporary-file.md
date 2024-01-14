---
title:                "C#: Tworzenie tymczasowego pliku"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami podczas pisania programów w C#, możemy napotkać potrzebę tworzenia plików tymczasowych. Dlaczego? Ponieważ tworzenie tymczasowych plików może być bardzo przydatne w wielu sytuacjach, takich jak przetwarzanie dużych danych, tymczasowe przechowywanie informacji czy testowanie funkcjonalności.

## Jak to zrobić

Istnieją różne sposoby na tworzenie tymczasowych plików w C#. Jednym z najprostszych sposobów jest użycie metody `Path.GetTempFileName()`. Przykład użycia tej metody można zobaczyć poniżej:

```C#
string tempFile = Path.GetTempFileName();
Console.WriteLine("Utworzono tymczasowy plik: " + tempFile);
```

W powyższym kodzie zostanie utworzony i wyświetlony na ekranie ścieżka do tymczasowego pliku. Możemy także określić własną nazwę dla pliku, wykorzystując klasę `Path` i otwierając strumień do tworzonego pliku:

```C#
string tempFile = Path.GetTempPath() + "moj_plik.tmp";
using (FileStream fs = File.Create(tempFile))
{
    Console.WriteLine("Utworzono tymczasowy plik: " + tempFile);
}
```

Warto również pamiętać, aby usunąć tymczasowy plik po zakończeniu jego wykorzystywania. Możemy to zrobić ręcznie, wywołując metodę `File.Delete()` lub korzystając z wyrażenia `using`:

```C#
string tempFile = Path.GetTempPath() + "moj_plik.tmp";
using (FileStream fs = File.Create(tempFile))
{
    Console.WriteLine("Utworzono tymczasowy plik: " + tempFile);
}
// Do something with the temp file
File.Delete(tempFile); // Usuwanie ręczne
//------------------------------------
using (FileStream fs = File.Create(tempFile))
{
    Console.WriteLine("Utworzono tymczasowy plik: " + tempFile);
}
// Do something with the temp file
// Plik zostanie automatycznie usunięty po opuszczeniu bloku "using"
```

## Deep Dive

Tworzenie tymczasowych plików może odbywać się na różnych poziomach abstrakcji. Na przykład, może to być jedynie operacja na poziomie pliku, jak w powyższych przykładach, lub też może być traktowane jako część większej operacji, takiej jak przetwarzanie danych w bazie lub wczytywanie plików z internetu. Należy jednak pamiętać, że pliki tymczasowe często zajmują miejsce na dysku twardym i nie powinno się nadużywać tworzenia zbyt wielu z nich.

## Zobacz także

- [Dokumentacja Path.GetTempFileName()](https://docs.microsoft.com/pl-pl/dotnet/api/system.io.path.gettempfilename)
- [Artykuł na temat tworzenia tymczasowych plików](https://www.c-sharpcorner.com/UploadFile/4d9083/working-with-tmp-file-in-C-Sharp/) (język angielski)
- [Dokumentacja klasy Path](https://docs.microsoft.com/pl-pl/dotnet/api/system.io.path)