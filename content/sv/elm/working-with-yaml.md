---
title:                "Arbeta med yaml"
html_title:           "Elm: Arbeta med yaml"
simple_title:         "Arbeta med yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

YAML är ett format för att strukturera data som är lätt att läsa och skriva. Det har blivit allt mer populärt och används ofta för konfigurationsfiler i mjukvaruprojekt. Genom att lära sig YAML kan du effektivt hantera och organisera din data i dina projekt.

## Så här gör du

För att kunna arbeta med YAML i Elm behöver du först installera paketet "elm-yaml" genom terminalen eller pakethanteraren. När paketet är installerat kan du börja använda YAML i ditt projekt.

För att skapa ett nytt YAML-dokument i Elm använder du funktionen `Yaml.encode`, vilket tar ett Elm-värde (record, list etc.) som indata och returnerar ett YAML-dokument som en sträng. Här är ett exempel:

```Elm
import Yaml exposing (..)

data =
    { name = "John"
    , age = 25
    , hobbies = ["photography", "hiking"]
    }

yaml = Yaml.encode data

-- yaml innehåller nu:
-- name: John
-- age: 25
-- hobbies:
--   - photography
--   - hiking
```

För att läsa in ett befintligt YAML-dokument i Elm använder du funktionen `Yaml.decode`, som tar en YAML-sträng som indata och returnerar ett Elm-värde. Här är ett exempel:

```Elm
import Yaml exposing (..)

yaml =
name: Jane
age: 30
hobbies:
    - painting
    - baking

result = Yaml.decode yaml

-- result innehåller nu:
-- { name = "Jane"
-- , age = 30
-- , hobbies = ["painting", "baking"]
-- }
```

## Djupdykning

Genom att lägga till YAML-filer i ditt projekt kan du på ett effektivt sätt hantera konfigurationer och data som behövs för att ditt program ska fungera korrekt. Du kan också använda YAML för att generera statiska webbsidor eller som del av ditt byggscript. Ytterligare möjligheter med YAML inkluderar att samla och analysa data samt att skapa strukturerade dokument.

För att lära dig mer om YAML kan du utforska dokumentationen för "elm-yaml" paketet och prova på olika funktioner och exempel. Det finns också många resurser online som ger en mer djupgående förståelse för YAML och dess användning i mjukvaruprojekt.

## Se även

- [elm-yaml paketet](https://package.elm-lang.org/packages/NoRedInk/elm-yaml/latest/)
- [YAML.org](https://yaml.org/)
- [Awesome Elm](https://github.com/sporto/awesome-elm) för fler resurser och verktyg för Elm-språket.