---
title:                "Praca z plikami csv"
html_title:           "Gleam: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pracowanie z plikami CSV jest nieodłączną częścią pracy programisty. CSV (Comma Separated Values) to prosty format plików, w którym dane są przechowywane w postaci tabelarycznej, z wartościami oddzielonymi przecinkami. Programiści korzystają z tego formatu, ponieważ jest łatwy do czytania i przechowywania danych w formie zrozumiałej dla komputera.

## Jak to zrobić?

Gleam to język programowania, który umożliwia nam łatwe wykonywanie operacji na plikach CSV. W poniższych przykładach przedstawimy prosty kod, który pomoże Ci w tym zadaniu. Pamiętaj, że w ramach przykładu mogą pojawić się błędy, które należy samodzielnie naprawić, aby kod zadziałał poprawnie.

```Gleam
// Importowanie biblioteki CSV
import gleam/csv

// Otworzenie pliku CSV
let file = csv.open("data.csv")

// Odczytanie danych z pliku
let data = csv.read(file)

// Wypisanie zawartości w konsoli
csv.print(data)
```

W rezultacie otrzymamy dane w tabelarycznej formie, gotowe do dalszej obróbki.

## Głębszy wgląd

Praca z plikami CSV jest bardzo popularna w programowaniu, ponieważ ten format był używany już od bardzo dawna i jest szeroko rozpoznawalny. Istnieją również alternatywne formaty takie jak JSON czy XML, jednak CSV jest często preferowany ze względu na prostotę i czytelność dla człowieka.

Jeśli chcesz dowiedzieć się więcej o tym, jak działają pliki CSV oraz jakie są ich możliwości i ograniczenia, koniecznie sprawdź linki w sekcji "Zobacz również".

## Zobacz również

- [CSV na Wikipedii](https://pl.wikipedia.org/wiki/CSV)