---
title:                "Praca z plikami CSV"
date:                  2024-01-19
html_title:           "Bash: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"

category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z CSV polega na manipulowaniu danymi w formacie Comma Separated Values – tekstowych plikach, gdzie wartości oddzielone są przecinkami. Programiści używają CSV, bo to prosty format do wymiany danych, wygodny w integracji z różnymi aplikacjami, często stosowany do eksportu i importu danych.

## Jak to zrobić:
Ładowanie danych z CSV do programu:
```C#
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main()
    {
        var csvData = File.ReadAllLines("dane.csv");
        foreach (var line in csvData)
        {
            var values = line.Split(',');
            Console.WriteLine($"Imię: {values[0]}, Wiek: {values[1]}");
        }
    }
}
```
Zapis danych do pliku CSV:
```C#
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main()
    {
        var users = new List<(string Name, int Age)>
        {
            ("Jan", 30),
            ("Anna", 25)
        };

        using var writer = new StreamWriter("eksport.csv");
        foreach (var user in users)
        {
            writer.WriteLine($"{user.Name},{user.Age}");
        }
    }
}
```
Przykładowe wyjście:
```
Imię: Jan, Wiek: 30
Imię: Anna, Wiek: 25
```

## Deep Dive
Format CSV nie jest jednoznacznie standardyzowany – różni się detalami w zależności od źródła. Historia jego powstania sięga wczesnych lat 70., kiedy zaczęto używać komputerów do przechowywania danych. Alternatywami dla CSV są m.in. XML i JSON, które są bardziej elastyczne, ale i bardziej złożone. Przy pracy z CSV ważne jest uwzględnienie obsługi cytatów, nowych linii w wartościach czy różnych separatorów.

## Zobacz także
- Dokumentacja Microsoftu o klasie [StreamWriter](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter)
- Dokumentacja Microsoftu o klasie [File](https://docs.microsoft.com/en-us/dotnet/api/system.io.file)
- Tutorial dotyczący biblioteki [CsvHelper](https://joshclose.github.io/CsvHelper/), jeśli potrzebujesz rozbudowanych możliwości pracy z CSV
- Oficjalna strona specyfikacji formatu [CSV](https://tools.ietf.org/html/rfc4180), choć nie jest to "pełny" standard.
