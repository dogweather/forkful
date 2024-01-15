---
title:                "Praca z json"
html_title:           "Haskell: Praca z json"
simple_title:         "Praca z json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach, gdzie wszystkie aplikacje i systemy wymieniają dane, kluczowym elementem jest wygodny sposób reprezentacji tych danych. JSON (JavaScript Object Notation) jest popularnym formatem danych, który jest powszechnie stosowany w aplikacjach webowych i mobilnych. Dzięki użyciu JSONa, możliwe jest szybkie i łatwe przesyłanie danych między różnymi platformami. Dlatego, warto poznać jak w prosty sposób pracować z tym formatem w języku Haskell.

## Jak To Zrobić

Do pracy z JSON w Haskellu będziemy potrzebować odpowiedniej biblioteki, takiej jak "aeson". Najpierw jednak musimy zainstalować odpowiedni pakiet z pomocą menadżera pakietów - "cabal" lub "stack".

```Haskell
cabal install aeson
```

Aby móc przetwarzać dane w formacie JSON, musimy najpierw przekonwertować je na odpowiednie typy danych w Haskellu. Najpowszechniejszym sposobem jest użycie funkcji `decode` z biblioteki "aeson", która zamienia wartość typu `ByteString` na wartość typu `Maybe Value`. Przykładowy kod wyglądałby następująco:

```Haskell
import Data.Aeson

jsonData = "{\"name\": \"John\", \"age\": 27}"

main = do
    let maybeValue = decode jsonData :: Maybe Value
    case maybeValue of
        Just value -> print value
        Nothing -> print "Parsowanie błędne!"
```

Powyższy kod najpierw dekoduje dane przekazane jako `ByteString` do typu `Maybe Value`, a następnie jeśli dekodowanie powiedzie się, wypisuje odpowiednią wartość. W przeciwnym wypadku, wypisuje informację o błędzie.

Możemy również w prosty sposób przetwarzać dane JSON w plikach. Przykładowy plik "dane.json" zawierający informacje o pracownikach może wyglądać tak:

```JSON
[
    {"name": "Anna", "age": 32, "position": "Developer"},
    {"name": "Marek", "age": 28, "position": "Tester"},
    {"name": "Kasia", "age": 25, "position": "Project Manager"}
]
```

Aby przetworzyć ten plik w Haskellu, musimy najpierw go oczytać, a następnie wykorzystać funkcję `decode`:

```Haskell
import Data.ByteString.Lazy
import Data.Aeson

main = do
    jsonData <- Data.ByteString.Lazy.readFile "dane.json"
    let maybeValue = decode jsonData :: Maybe Value
    case maybeValue of
        Just value -> print value
        Nothing -> print "Parsowanie błędne!"
```

Oczywiście, dekodowane dane możemy przypisać już do konkretnych typów danych, np. listy pracowników.

```Haskell
import Data.ByteString.Lazy
import Data.Aeson

data Employee = Employee {name :: String, age :: Int, position :: String} deriving (Show)

instance FromJSON Employee where
    parseJSON (Object v) = Employee <$> v .: "name" <*> v .: "age" <*> v .: "position"
    -- w powyższym przypadku, nazwy pól w typie "Employee" muszą odpowiadać nazwom kluczy w danych JSON

main = do
    jsonData <- Data.ByteString.Lazy.readFile "dane.json"
    let maybeEmployees = decode jsonData :: Maybe [Employee]
    case maybeEmployees of
        Just employees -> print employees
        Nothing -> print "Parsowanie błędne!"
```

Warto również wspomnieć o funkcji `encode`, która działa w drugą stronę - zamienia wartości typów danych Haskell na reprezentację JSON.

## Deep Dive

Aby łatwiej manipulować danymi typu `Value`, istnieje wiele przydatnych funkcji w bibliotece "aeson", takich jak `object`, `array`, `string`, `number`, `bool`. Moż