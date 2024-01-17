---
title:                "Odczytywanie pliku tekstowego"
html_title:           "C#: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego to robimy?

Czytasz ten artykuł, co oznacza, że już wiesz, jak ważne jest czytanie. Przeczytanie tekstu jest również ważne dla programistów. W skrócie, czytanie pliku tekstowego to po prostu odczytanie zawartości pliku zapisanego w postaci tekstu.

Programiści często czytają pliki tekstowe, ponieważ są one powszechnie wykorzystywane do przechowywania danych, takich jak ustawienia programu czy dane wejściowe dla aplikacji. Czytanie plików tekstowych pozwala programistom na dostęp i manipulację tymi danymi.

## Jak to zrobić:

```C#
// Przykładowe kodowanie w C# do odczytania pliku tekstowego
string[] lines = System.IO.File.ReadAllLines(@"C:\scieżka\do\pliku.txt");
foreach (string line in lines)
{
    Console.WriteLine(line);
}
```

Powyższy kod używa metody `ReadAllLines` z klasy `File` z przestrzeni nazw `System.IO`. Ta metoda odczytuje cały plik tekstowy i zwraca jego zawartość jako tablicę napisów. Następnie w pętli `foreach` każda linia tekstu jest wyświetlana na konsoli.

Przykładowy plik tekstowy może wyglądać tak:

```
Witaj!
To przykładowy plik tekstowy.
Jest to druga linia.
A to trzecia!
```

Po uruchomieniu przykładowego kodu, na konsoli zostanie wyświetlony następujący wynik:

```
Witaj!
To przykładowy plik tekstowy.
Jest to druga linia.
A to trzecia!
```

## Głębszy zanurzenie:

Czytanie plików tekstowych jest jednym z najprostszych sposobów na dostęp do danych w programowaniu. W przeszłości, gdy jeszcze nie było baz danych, pliki tekstowe były jedynym sposobem na przechowywanie danych. W dzisiejszych czasach inne metody, takie jak bazy danych czy sieciowe API, są często wykorzystywane do dostępu do danych. Jednak pliki tekstowe nadal są popularne ze względu na prostotę ich użycia.

Alternatywnym podejściem do czytania plików tekstowych może być wykorzystanie klasy `StreamReader` z przestrzeni nazw `System.IO`. Klasa ta pozwala na odczyt pliku linia po linii, co może być przydatne przy dużych plikach, gdy nie chcemy wczytywać całego pliku na raz.

Implementacja odczytywania plików tekstowych w C# jest oparta na platformie .NET Framework, więc jest to dostępne dla programistów na platformach Windows i Linux.

## Zobacz też:

Oficjalna dokumentacja Microsoft dla klasy `File`: https://docs.microsoft.com/pl-pl/dotnet/api/system.io.file <br/>
Oficjalna dokumentacja Microsoft dla klasy `StreamReader`: https://docs.microsoft.com/pl-pl/dotnet/api/system.io.streamreader