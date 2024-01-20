---
title:                "Praca z formatem json"
html_title:           "Haskell: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Praca z JSON-em to nic innego jak manipulowanie danymi w formacie JavaScript Object Notation. Format ten jest używany przez programistów do przesyłania i przechowywania danych, ze względu na jego prostotę i czytelność dla człowieka.

## Jak to zrobić:
### Odczyt
Aby odczytać dane z pliku JSON w Haskellu, użyj biblioteki *aeson*. Najpierw należy zaimportować odpowiednie moduły:

```Haskell
import Data.Aeson
import Data.ByteString.Lazy
import GHC.Generics
```
Następnie, należy zdefiniować typ danych, który odpowiada strukturze JSON-a:

```Haskell
data Person = Person { name :: String, age :: Int } deriving (Show, Generic)
```

Następnie, możemy odczytać plik JSON i zamienić go na odpowiednią wartość:

```Haskell
json <- readFile "plik.json"
let maybePerson = decode json :: Maybe Person
```

### Zapis
Aby zapisać dane do pliku JSON, należy najpierw stworzyć odpowiednią wartość, a następnie przekonwertować ją na format JSON:

```Haskell
let person = Person { name = "Jan", age = 25 }
let json = encode person
writeFile "plik.json" json
```

## Wszystko wiadomo?
### Kontekst historyczny
JSON został stworzony przez Douglasa Crockforda w latach 90-tych jako prosty format wymiany danych. Z czasem, stał się on bardzo popularny w świecie programowania.

### Alternatywy
Alternatywami dla JSON-a w Haskellowi są na przykład formaty CSV lub XML. Jednakże, JSON jest najczęściej używanym formatem ze względu na swoją prostotę i czytelność.

### Detale implementacyjne
Biblioteka *aeson* wykorzystuje mechanizm generycznych typów danych w Haskellu, aby automatycznie generować instancje do konwersji pomiędzy typami danych a formatem JSON.

## Zobacz też:
- [Aeson documentation](http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html)