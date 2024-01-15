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

## Dlaczego 

CSV (ang. Comma Separated Values) to popularny format pliku używany do przechowywania i przetwarzania danych. W tym artykule dowiesz się, dlaczego warto nauczyć się pracować z CSV i jak można to zrobić w języku C#.

## Jak To Zrobić

Tworzenie plików CSV w języku C# jest bardzo proste i wymaga niewielkiego wysiłku. Wystarczy użyć dwóch prostych kroków:

1. Zainstaluj bibliotekę CsvHelper za pomocą menedżera pakietów NuGet:

```C#
Install-Package CsvHelper
```

2. Napisz kod do odczytu lub zapisu pliku CSV:

```C#
using (var reader = new StreamReader("dane.csv"))
using (var csv = new CsvReader(reader, CultureInfo.InvariantCulture))
{
    var records = csv.GetRecords<Dane>(); // Dane to klasa, która będzie reprezentować strukturę danych w pliku CSV
}
```

To wszystko! Teraz możesz łatwo odczytywać i zapisywać dane w plikach CSV.

Przykładowy output dla pliku CSV o strukturze: `"Imię","Nazwisko","Wiek"`:

```C#
using (var writer = new StreamWriter("dane.csv"))
using (var csv = new CsvWriter(writer, CultureInfo.InvariantCulture))
{
    csv.WriteRecords(new List<Dane>
    {
        new Dane { Imię = "Jan", Nazwisko = "Kowalski", Wiek = 35 },
        new Dane { Imię = "Anna", Nazwisko = "Nowak", Wiek = 28 },
        new Dane { Imię = "Piotr", Nazwisko = "Lis", Wiek = 41 },
    });
}
```

Spójrz, co zostało zapisane w pliku CSV:

```
Jan,Kowalski,35
Anna,Nowak,28
Piotr,Lis,41
```

## Deep Dive

W języku C# istnieje wiele bibliotek i narzędzi, które mogą ułatwić pracę z plikami CSV. Jedną z popularniejszych jest już wcześniej wspomniana CsvHelper, która oferuje wiele funkcji, takich jak automatyczne mapowanie pól z klasy do kolumn w pliku CSV czy obsługa różnych typów danych.

Ponadto, niektóre z funkcji wbudowanych w język C#, takie jak LINQ, mogą być również użyteczne w manipulacji danych z plików CSV. Nie zapomnij także o wcześniejszej walidacji danych za pomocą mechanizmów takich jak atrybuty w klasie.

Warto również zwrócić uwagę na wydajność operacji na plikach CSV - przy pracy z dużymi plikami, zaleca się używanie funkcji streamingu i wykorzystanie pakietów, które pozwalają na przetwarzanie danych partiami (tzw. batch processing).

## Zobacz Również

- [Oficjalna dokumentacja CsvHelper](https://joshclose.github.io/CsvHelper/)
- [Przykładowe aplikacje w języku C# z użyciem CsvHelper](https://github.com/topicusonderwijs/csvhelper-examples)
- [Jak walidować dane w plikach CSV w języku C#](https://exceptionnotfound.net/validating-csv-files-against-different-classes-using-csvhelper/)

Dziękujemy za przeczytanie tego artykułu. Mamy nadzieję, że teraz już wiesz, jak łatwo i przyjemnie można pracować z plikami CSV w języku C#!