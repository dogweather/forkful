---
title:                "Parsowanie daty z ciągu znaków"
html_title:           "C#: Parsowanie daty z ciągu znaków"
simple_title:         "Parsowanie daty z ciągu znaków"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# C# - Analiza daty ze stringa

## Co i dlaczego?
Analiza daty ze stringa to proces konwertowania tekstu zawierającego informacje o dacie w odpowiedni obiekt daty, który można w łatwy sposób manipulować. Programiści często wykorzystują tę funkcję w celu przetwarzania danych wejściowych lub wyświetlania dat w różnych formatach.

## Jak to zrobić?
Aby analizować datę ze stringa w C#, należy użyć metody ```DateTime.Parse()``` lub ```DateTime.ParseExact()```. Przykładowy kod wygląda następująco:
```
string input = "2019-05-22";
DateTime date = DateTime.Parse(input);
Console.WriteLine(date);
```
Output:
```
22.05.2019 00:00:00
```
Można również określić niestandardowy format daty za pomocą ```DateTime.ParseExact()```:
```
string input = "2019-05-22";
DateTime date = DateTime.ParseExact(input, "yyyy-MM-dd", CultureInfo.InvariantCulture);
Console.WriteLine(date);
```
Output:
```
22.05.2019 00:00:00
```

## Wnikliwe spojrzenie
Parsowanie daty ze stringa jest wykorzystywane od dawna w programowaniu. W przeszłości, programiści musieli ręcznie konwertować stringi na daty, co było czasochłonne i podatne na błędy. Alternatywą jest wykorzystanie typu danych ```DateTimeOffset```, który przechowuje informacje o dacie i czasie wraz z informacjami o strefie czasowej.

Implementacja metody ```DateTime.Parse()``` jest dość prosta, ponieważ przekształca ona stringa na elementy składowe daty, takie jak dzień, miesiąc i rok. Następnie te elementy są przekształcane na datę z użyciem kalendarza gregoriańskiego.

## Zobacz także
- Dokumentacja Microsoft dotycząca parsowania daty w C#: https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime.parse
- Wprowadzenie do typu danych DateTimeOffset: https://docs.microsoft.com/pl-pl/dotnet/api/system.datetimeoffset
- Wideo na temat analizowania daty w C#: https://www.youtube.com/watch?v=mRw0G4Mbs8I