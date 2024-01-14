---
title:                "Haskell: Att arbeta med json"
simple_title:         "Att arbeta med json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Varför

JSON (JavaScript Object Notation) är ett populärt format för att överföra data mellan olika system, inklusive webbsidor och webbapplikationer. Att kunna hantera JSON i Haskell öppnar upp möjligheter för att skapa kraftfulla och flexibla applikationer som kan kommunicera med andra system på ett effektivt sätt.

## Hur man gör

För att kunna arbeta med JSON i Haskell behöver du först importera "Data.Aeson" biblioteket. Detta bibliotek innehåller många användbara funktioner för att konvertera JSON-data till Haskell-datatyper och vice versa.

Ett enkelt sätt att representera JSON-data i Haskell är genom att använda "Value" typen. Den kan representera alla olika typer av JSON-data, som objekt, listor, strängar, booleska värden osv.

```haskell
-- Importera Data.Aeson biblioteket
import Data.Aeson

-- En exempel JSON-sträng
jsonStr = "{\"name\":\"Anna\", \"age\": 25, \"hobbies\":[\"gaming\", \"painting\"]}"

-- Konvertera JSON-strängen till en "Value" typ
jsonVal = decode jsonStr :: Maybe Value 

-- Skriva ut JSON-värdet
print jsonVal 
-- Resultat: Just (Object (fromList [("name",String "Anna"),("age",Number 25.0),("hobbies",Array [(String "gaming"),(String "painting")])]))
```

Som du kan se konverterades JSON-strängen till en "Value" typ och skrevs sedan ut. Men för att kunna arbeta med data på ett mer strukturerat sätt kan vi använda datatyper som motsvarar olika typer av JSON-data. Till exempel, för att representera ett JSON-objekt med namn, ålder och en lista av hobbies kan vi använda "fromJSON" funktionen för att konvertera "Value" typen till en "Maybe" typ som motsvarar vår egen datatyp.

```haskell
-- Skapa en egendefinierad datatyp för vårt JSON-objekt
data Person = Person {
  name :: String,
  age :: Int,
  hobbies :: [String]
} deriving (Show, Eq) -- Derivande "Show" och "Eq" gör att vi kan skriva ut och jämföra värden av denna typ

-- Definiera en instans av "FromJSON" för vår Person typ
instance FromJSON Person where
  parseJSON (Object v) = Person
    <$> v .: "name" -- Värdet som motsvarar "name" nyckeln konverteras till en sträng och används för "Person" namn
    <*> v .: "age" -- Värdet som motsvarar "age" nyckeln konverteras till en "Int" och används för "Person" ålder
    <*> v .: "hobbies" -- Värdet som motsvarar "hobbies" nyckeln konverteras till en lista av strängar och används för "Person" hobbies
  parseJSON _ = mzero -- Om värdet inte matchar förväntat format returneras "mzero"

-- Skapa en "Person" från vårt JSON-värde
personVal = decode jsonStr :: Maybe Person

-- Skriva ut personen
print personVal 
-- Resultat: Just (Person {name = "Anna", age = 25, hobbies = ["gaming","painting" ]})
```

Nu har vi en mer strukturerad representation av vårt JSON-objekt som vi kan arbeta med på ett enklare sätt.

## Djupdykning

Genom att använda "Data.Aeson" biblioteket kan du hantera mer komplexa JSON-strukturer och göra olika operationer som att filtrera, uppdatera och skapa nya JSON-data. Det finns också många andra bibliotek som bygger på "Data.Aeson" för att ge mer funktionalitet och bekvämlighet när man arbetar med JSON i Haskell.

## Se även

- Haskell Wiki: https://wiki.haskell.org/json
- Aeson dokumentation: https://hackage.haskell.org/package/aeson
- Haskell kurs på Codecademy: https://www.codecademy.com/learn