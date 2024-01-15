---
title:                "Praca z plikami csv"
html_title:           "Elm: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego
Dlaczego chciałbyś pracować z plikami CSV? Te pliki zawierają dane w formacie tabeli, który jest powszechnie stosowany w biznesie i naukach, więc znajomość ich obsługi jest kluczowa dla wielu zawodów. W tym artykule dowiesz się, jak w prosty sposób pracować z plikami CSV w języku Elm.

## Jak to zrobić
```Elm
-- Odczytanie pliku CSV
fileContent : String
fileContent =
  """
  Imię,Nazwisko,Wiek
  Maria,Kowalska,34
  Jan,Nowak,42
  """

parseCSV : List (List String)
parseCSV =
  fileContent
    |> String.split "\n"
    |> List.map (String.split ",")
```
Kod powyżej pokazuje, jak wczytać i sparsować plik CSV w języku Elm. Korzystając z funkcji String.split, możemy podzielić dane na odpowiednie kolumny i rzędy. Następnie, dzięki wykorzystaniu funkcji List.map, dane są mapowane na listy, co pozwala nam na łatwiejszą manipulację nimi.

Aby zapisać zmiany w pliku CSV, możemy wykorzystać funkcję String.join, która pozwala na połączenie listy danych z rozdzielającym je separatorem, takim jak ",". Następnie możemy zapisać zmodyfikowany string do pliku.

## Głębszy zanurzenie
W języku Elm istnieją również gotowe paczki, które ułatwiają pracę z plikami CSV, takie jak elm-csv i elm-csv-decode. Dzięki nim możemy wykorzystać zaawansowane funkcje, takie jak dekodowanie danych do odpowiednich typów.

Ponadto, niektóre biblioteki, takie jak ohi/csv-decode, umożliwiają odczytanie i dekodowanie plików CSV bezpośrednio z linków internetowych, co może być przydatne podczas pracy z danymi z zewnętrznych źródeł.

## Zobacz również
- Dokumentacja języka Elm: https://elm-lang.org/docs
- Paczka elm-csv: https://package.elm-lang.org/packages/elm/parser/latest/
- Biblioteka ohi/csv-decode: https://package.elm-lang.org/packages/ohanhi/elm-csv-decode/latest/