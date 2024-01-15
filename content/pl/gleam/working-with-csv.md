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

## Dlaczego

Praca z plikami CSV jest powszechnym zadaniem w programowaniu i może się przydać w wielu sytuacjach, na przykład podczas importowania danych, przetwarzania raportów, czy w przypadku analizy danych. W Gleam, biblioteka CSV jest bardzo łatwa w użyciu i pozwala na szybkie i skuteczne operowanie na danych w tym formacie.

## Jak to zrobić

```Gleam
import gleam/csv
csv.parse("data.csv")
  |> json.toJson
  |> io.print
```

W powyższym przykładzie korzystamy z biblioteki CSV, aby sparsować plik CSV i przekonwertować go do formatu JSON. Następnie wyświetlamy wynik przy użyciu funkcji io.print. Możliwości manipulacji danymi są praktycznie nieograniczone dzięki łatwej i intuicyjnej składni Gleam.

## Głębszy wgląd

W bibliotece CSV znajdują się funkcje umożliwiające wczytywanie danych z różnych źródeł, takich jak URL, pliki lokalne czy dane w formacie binarnym. Poza tym, można również zmieniać separator kolumn oraz dostosowywać sposób konwersji danych. Biblioteka jest również często aktualizowana i ulepszana przez społeczność programistów, dzięki czemu jest jeszcze bardziej przyjazna dla użytkowników.

## Zobacz także

- [Oficjalna dokumentacja biblioteki CSV w Gleam](https://gleam.run/libraries/csv)
- [Repozytorium GitHub z kodem źródłowym biblioteki CSV w Gleam](https://github.com/lpil/csv)
- [Przykłady kodu i poradniki udostępniane przez społeczność użytkowników Gleam](https://gleam.run/community)