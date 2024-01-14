---
title:                "Elm: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

CSV, czyli format pliku wartości rozdzielanych przecinkami, jest bardzo popularnym sposobem przechowywania danych w formie tabelarycznej. Jego prosty i czytelny format sprawia, że jest powszechnie stosowany w wielu dziedzinach, takich jak biznes, nauka czy technologia. Dlatego warto nauczyć się jak pracować z CSV w Elm, aby móc wygodnie analizować i manipulować danymi w swoich projektach.

## Jak to zrobić

Aby zacząć pracę z CSV w Elm, potrzebujemy użyć biblioteki o nazwie Csv Encode, która jest dostępna poprzez menedżera pakietów. Następnie, aby odczytać plik CSV, możemy skorzystać z funkcji `Decode.csv` i podać jej ścieżkę do pliku oraz strukturę danych, do której chcemy zapisać odczytane wartości. Poniżej przedstawiam przykładowy kod:

```Elm
import Csv.Encode as Csv
import Csv.Decode as Csv
import File exposing (Picker)
import Json.Encode

-- Odczytanie danych z pliku CSV
fileDecoder : Csv.Decoder (List String)
fileDecoder =
    Csv.row

-- Przykładowa funkcja zapisująca wynik do pliku JSON
saveToJson : Picker.File -> Cmd Msg 
saveToJson file =
  picker "Wybierz plik CSV" (Task.map3 saveToData fileDecoder Csv.encode file)

-- Przykładowy plik CSV
file : Csv.Encoding
file =
  Csv.encoding
    [ "Imię, Nazwisko, Wiek"
    , "Anna, Nowak, 35"
    , "Piotr, Kowalski, 28"
    , "Marta, Wiśniewska, 42"
    ]

-- Przykładowy wynik zapisany do pliku JSON
result : Json.Encode.Value
result =
  Json.encode file

```

W powyższym przykładzie, najpierw importujemy potrzebne moduły, a następnie definiujemy funkcję `fileDecoder`, która jest odpowiedzialna za dekodowanie pliku CSV do listy ciągów znaków (w tym przypadku linii w pliku). Następnie wykorzystujemy funkcję `picker` z modułu `File`, aby wybrać plik CSV z naszego systemu plików, a za pomocą funkcji `Task.map3` połączyć wynik z funkcji `saveToData` z kodowaniem CSV i tym wybranym plikiem. Na końcu, aby zapisać wynik do pliku JSON, wykorzystujemy funkcję `Json.encode`. 

## Deep Dive

W Elm, mamy duży wybór bibliotek do pracy z CSV, w zależności od naszych potrzeb i oczekiwań. Możemy wykorzystać np. bibliotekę Csv-Parser, aby pracować ze złożonymi plikami CSV z nagłówkami i kolumnami, lub Csv-Parser-Light dla prostszych przypadków. Istnieją również inne pomocne biblioteki, takie jak Csv-Index, która umożliwia wyszukiwanie konkretnych wartości w pliku CSV.

Kluczową kwestią przy pracy z CSV w Elm jest również radzenie sobie z ewentualnymi błędami, na przykład gdy plik CSV zawiera nieprawidłowe dane lub jest uszkodzony. Dlatego warto zwrócić uwagę na funkcje dostępne w bibliotekach, takie jak `Decode.OnError`, które pozwalają na elegancką obsługę błędów w naszym kodzie.

## Zobacz również

- [Dokumentacja biblioteki Csv-Encode](https://package.elm-lang.org/packages/zgohr/elm-csv-encode/latest/Csv-Encode)
- [Dokumentacja biblioteki Csv-Decode](https://package.elm-lang.org/packages/zgohr/elm-csv-decode/latest/Csv-Decode)
- [Przykład wykorzystania biblioteki Csv-Parser](https://elmprogramming.com/parsing-csv-files-in-elm