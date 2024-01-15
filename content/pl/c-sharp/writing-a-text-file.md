---
title:                "Pisanie pliku tekstowego"
html_title:           "C#: Pisanie pliku tekstowego"
simple_title:         "Pisanie pliku tekstowego"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Zapisywanie pliku tekstowego to podstawowa, ale niezbędna umiejętność w programowaniu. Dzięki temu możemy przechowywać i edytować dane, które są niezbędne dla naszych programów. Bez niego, nasze aplikacje nie mogłyby wykorzystywać informacji, co często prowadziłoby do niezawodności i niepełnej funkcjonalności.

## Jak to zrobić

Aby zapisać plik tekstowy w języku C#, musimy najpierw wykorzystać przestrzeń nazw System.IO, która umożliwia nam dostęp do klas i metod związanych z operacjami wejścia/wyjścia. Następnie, możemy użyć klasy StreamWriter, która pozwala nam pisać do pliku tekstowego. Przykład kodu poniżej pokazuje, jak zapisać trzy linie tekstu do pliku o nazwie "tekstowy.txt".

```C#
using System.IO;

StreamWriter sw = new StreamWriter("tekstowy.txt");
sw.WriteLine("Pierwsza linia tekstu.");
sw.WriteLine("Druga linia tekstu.");
sw.WriteLine("Trzecia linia tekstu.");
sw.Close();
```

Po uruchomieniu tego kodu, zostanie utworzony plik tekstowy o nazwie "tekstowy.txt", a w nim będą zawarte trzy linie tekstu, oddzielone nowymi liniami.

## Głębsze wędrówki

Zapisywanie pliku tekstowego może obejmować także bardziej wymagające czynności, takie jak odczytywanie danych z zewnętrznych źródeł lub formatowanie tekstu w celu uzyskania czytelniejszego wyglądu pliku. W takich przypadkach, możemy wykorzystać inne klasy i metody dostępne w przestrzeni nazw System.IO, aby manipulować danymi i tworzyć bardziej zaawansowane pliki tekstowe.

## Zobacz także

- [Jak odczytać plik tekstowy w C#](https://docs.microsoft.com/pl-pl/dotnet/csharp/programming-guide/file-system/how-to-read-from-a-text-file)
- [Podstawy operacji wejścia/wyjścia w C#](https://docs.microsoft.com/pl-pl/dotnet/standard/io/)
- [Inne sposoby na zapisywanie danych w C#](https://www.c-sharpcorner.com/UploadFile/mgold/ReadWriteDataTxtFile12062005003829AM/ReadWriteDataTxtFile.aspx)