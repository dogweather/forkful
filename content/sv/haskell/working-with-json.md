---
title:                "Arbeta med json"
html_title:           "Haskell: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Varför
Haskell är ett kraftfullt programmeringsspråk med optimerad prestanda och stark typning, vilket gör det till ett utmärkt val för att arbeta med JSON-data. Med hjälp av Haskell kan du enkelt och effektivt hantera komplexa JSON-strukturer och göra konverteringen till och från andra dataformat enkel och smidig.

## Hur man gör
För att arbeta med JSON i Haskell, behöver du först importera paketet Aeson. Detta paket innehåller en mängd olika funktioner som används för att analysera, bygga och manipulera JSON-data.

```Haskell
import Data.Aeson
```

### Skapa JSON
Du kan enkelt skapa en JSON-värde genom att använda en av funktionerna `object`, `array`, `string`, `number` eller `bool`. Till exempel:

```Haskell
-- En JSON-objekt med två attribut
object [("name", string "John"), ("age", number 30)]

-- En JSON-array med tre värden
array [string "apple", string "orange", string "banana"]

-- En JSON-sträng
string "Hello, world!"

-- En JSON-siffra
number 42

-- En JSON-boolesk värde
bool True
```

### Analys av JSON
För att analysera en JSON-sträng till ett Haskell-värde använder du funktionen `decode`. Om strängen inte kan parseras till ett giltigt JSON-värde, kommer funktionen att returnera `Nothing`.

```Haskell
decode "{\"name\": \"John\", \"age\": 30}" :: Maybe Object
-- Resultat: Just (fromList [("name",String "John"),("age",Number 30.0)])
```

### Åtkomst av attribut
Du kan använda funktionen `at` för att plocka ut ett specifikt attribut från ett JSON-objekt. Om attributet inte finns eller inte kan omvandlas till önskat typ, kommer funktionen att returnera `Nothing`.

```Haskell
obj <- decode "{\"name\": \"John\", \"age\": 30}" :: Maybe Object
obj .: "name" :: Maybe Text
-- Resultat: Just "John"
```

### Konvertering till och från JSON
För att konvertera ett Haskell-värde till JSON och vice versa, använd funktionerna `toJSON` och `fromJSON`. Dessa funktioner kan hantera en mängd olika datastrukturer, inklusive dina egna datatyper.

```Haskell
-- En enkel datatyp
data Person = Person { name :: String, age :: Int } deriving (Generic, Show)
instance ToJSON Person
instance FromJSON Person

-- Konvertera till JSON
toJSON (Person "John" 30)
-- Resultat: Object (fromList [("name",String "John"),("age",Number 30.0)])

-- Konvertera från JSON
fromJSON (Object (fromList [("name",String "John"),("age",Number 30.0)])) :: Result Person
-- Resultat: Success (Person {name = "John", age = 30})
```

## Djupdykning
Ett av de mest kraftfulla verktygen för att arbeta med JSON i Haskell är lens-bibiloteket. Med lens kan du enkelt åtkomma och manipulera datastrukturer av olika komplexitet, inklusive JSON.

```Haskell
import Control.Lens
import Data.Aeson.Lens

-- En enkel JSON-sträng
str <- "{\"name\": \"John\", \"age\": 30 }"

-- Använda lens för att plocka ut attributet "name"
str ^? key "name" :: Maybe Value
-- Resultat: Just (String "John")

-- Använda lens för att modifiera ålder i JSON-data
str & key "age" .~ Number 31 :: Value
-- Resultat: Object (fromList [("name",String "John"),("age",Number 31.0)])
```

## Se även
- Officiell dokumentation för Aeson-paketet: https://hackage.haskell.org/package/aeson
- Lens-bibliotekets hemsida: https://hackage.haskell.org/package/lens
- En tutorial om att arbeta med JSON i Haskell: https://www.fpcomplete.com/haskell/library/aeson/