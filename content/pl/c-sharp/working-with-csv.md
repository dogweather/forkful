---
title:                "Praca z plikami csv"
html_title:           "C#: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## O czym & Dlaczego?
Praca z plikami CSV (Comma Separated Value) polega na odczytywaniu i zapisywaniu danych w formacie tabelarycznym, gdzie każda komórka jest oddzielona znakiem przecinka. Programiści często pracują z CSV, ponieważ jest to szybki i prosty sposób na przechowywanie i przesyłanie danych, szczególnie w przypadku dużych ilości danych.

## Jak to zrobić?
Najpierw musisz dodać przestrzeń nazw "System.IO" do swojego kodu C#. Następnie użyj klasy "StreamReader" do odczytania danych z pliku CSV za pomocą metody "ReadLine()". Ponadto, klasa "StreamWriter" może być wykorzystana do zapisania danych do pliku CSV za pomocą metody "WriteLine()". W przykładzie poniżej pokazano jak odczytać dane z pliku CSV i wyświetlić je w konsoli:

```C#
using System.IO;

string line;
StreamReader csvFile = new StreamReader("nazwa_pliku.csv");

while ((line = csvFile.ReadLine()) != null)
{
    Console.WriteLine(line);
}

csvFile.Close();

// Output:
// Kolumna1, Kolumna2, Kolumna3
// Wartość1, Wartość2, Wartość3
// Wartość4, Wartość5, Wartość6
```

## Głębszy zanurzenie
Pliki CSV zostały wprowadzone w latach 70. i są powszechnie używane do przechowywania danych w formacie tabelarycznym. Alternatywą dla CSV są bazy danych, ale pliki CSV są szybsze i łatwiejsze do przetwarzania. Możesz również odczytywać i zapisywać dane w formacie CSV za pomocą biblioteki "CSVHelper", która oferuje dodatkowe funkcje, takie jak automatyczne mapowanie danych do modeli obiektów.

## Zobacz również
- Dokumentacja Microsoft C# o klasie StreamReader: https://docs.microsoft.com/pl-pl/dotnet/api/system.io.streamreader?view=net-5.0
- Dokumentacja Microsoft C# o klasie StreamWriter: https://docs.microsoft.com/pl-pl/dotnet/api/system.io.streamwriter?view=net-5.0
- Biblioteka CSVHelper: https://joshclose.github.io/CsvHelper/