---
title:                "Praca z plikami csv"
html_title:           "PowerShell: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## Co & dlaczego?
CSV (ang. Comma-separated values) to popularny format danych, który jest wykorzystywany przez programistów do przechowywania i przetwarzania informacji w postaci tabel. Jest to prosta i czytelna struktura, która pozwala na łatwe manipulowanie danymi, zarówno przez ludzi jak i przez komputery.

## Jak to zrobić:
Skorzystaj z poniższego kodu, aby wczytać plik CSV i przetworzyć go w przydatną strukturę danych:
```
PowerShell 
Import-Csv -Path "C:\Users\Marta\Documents\PrzykladowyPlik.csv" | ForEach-Object {
    # Tutaj możesz wykonać dowolne operacje na danych
    $_."Kolumna1"  # Przykład wypisania danej kolumny
}
```

Przykładowy wynik:
```
Wiersz1: Kolumna1 = wartość1, Kolumna2 = wartość2, ...
Wiersz2: Kolumna1 = wartość1, Kolumna2 = wartość2, ...
...
```

## Głębszy zanurzenie:
CSV ma długą historię, sięgającą początków komputerów osobistych w latach 70. XX wieku. Obecnie jest najpopularniejszym formatem do przechowywania danych tabelarycznych, ponieważ jest łatwo czytelny i obsługiwany przez większość programów biurowych i baz danych.

Alternatywą dla CSV może być format JSON, ale jest on bardziej skomplikowany i czasami trudniejszy do przetwarzania. W przypadku dużych zbiorów danych, lepszym wyborem może być wykorzystanie baz danych, które wspierają indeksowanie i szybkie wyszukiwanie.

Implementacja CSV w PowerShell jest oparta na Cmdletach Import-Csv i Export-Csv, które ułatwiają wczytywanie i zapisywanie danych CSV. Istnieją także inne narzędzia i biblioteki dostępne w internecie, jeśli potrzebujesz bardziej zaawansowanych funkcji.

## Zobacz też:
[Dokumentacja Microsoft o pracowaniu z plikami CSV w PowerShell](https://docs.microsoft.com/pl-pl/powershell/scripting/samples/working-with-csv-files?view=powershell-7.1)

[Darmowy kurs o przetwarzaniu danych w PowerShell](https://channel9.msdn.com/Series/GetStartedPowerShell3/Introduction-and-where-to-get-PowerShell-30?l=U2LGL5znEd9cDzXzCw6DyA%2Fbzb5Wg%2F4RVtZJ3jzlgxln1hXqZm%2FWTeeGlQbAHPQid9zud8VDb2ytdLoSdNll8gLrMgyeARFzNjwJ7Uqrrc2gsmXdDbiryVZldNeICT)