---
title:                "Praca z json"
html_title:           "Elm: Praca z json"
simple_title:         "Praca z json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

JSON (JavaScript Object Notation) jest popularnym formatem do przechowywania i przesyłania danych w aplikacjach internetowych. W połączeniu z językiem Elm, który jest przeznaczony do budowania silnych i niezawodnych interfejsów użytkownika, można wykorzystać JSON do tworzenia dynamicznych i interaktywnych aplikacji internetowych.

## Jak To Zrobić

Aby rozpocząć pracę z JSON w Elm, musimy najpierw zaimportować moduł [Json.Decode](https://package.elm-lang.org/packages/elm/json/latest) przy użyciu słowa kluczowego `import`. Następnie możemy użyć funkcji `decodeString` lub `decodeValue` do parsowania danych JSON na odpowiadające im typy danych w Elm.

```elm
import Json.Decode exposing (..)

type alias User =
    { name : String
    , age : Int
    }

decodeUser : Decoder User
decodeUser =
    map2 User
        (field "name" string)
        (field "age" int)

jsonString = """
{
    "name" : "John",
    "age" : 27
}
"""

result = decodeString decodeUser jsonString

-- output:
-- Ok ({ name = "John", age = 27 } : User)
```

W powyższym przykładzie użyliśmy funkcji `map2`, aby przekształcić wynik funkcji `field` na typ `User`. Możemy również użyć innych funkcji, takich jak `map`, `andThen` i `oneOf`, aby modyfikować wynik parsowania w zależności od naszych potrzeb.

## Głębszy Wgląd

Istnieje wiele innych metod, które możemy wykorzystać do przetwarzania danych JSON w Elm. Jedną z takich funkcji jest `at`, która pozwala nam pobierać dane zagnieżdżone wewnątrz obiektów.

```elm
type alias Address =
    { street : String
    , city : String
    , country : String
    }

type alias User =
    { name : String
    , age : Int
    , address : Address
    }

decodeUser : Decoder User
decodeUser =
    map3 User
        (field "name" string)
        (field "age" int)
        (at [ "address" ] address)

-- output:
-- Ok ({ name = "John", age = 27, address = { street = "Main St.", city = "New York", country = "USA" } } : User)
```

### Tablice w JSON

W przypadku, gdy mamy do czynienia z tablicami w JSON, możemy skorzystać z funkcji `list` lub `index` do parsowania danych do listy lub pobrania konkretnego elementu z listy.

```elm
type alias Country =
    { name : String
    , population : Int
    }

decodeCountry : Decoder Country
decodeCountry =
    map2 Country
        (field "name" string)
        (field "population" int)

decodeCountries : Decoder (List Country)
decodeCountries =
    list decodeCountry

jsonString = """
[
    {
        "name" : "Poland",
        "population" : 37979332
    },
    {
        "name" : "Germany",
        "population" : 83019200
    }
]
"""

result = decodeString decodeCountries jsonString

-- output:
-- Ok ([ { name = "Poland", population = 37979332 } <~> { name = "Germany", population = 83019200 } ] : List Country)
```

## Zobacz też

- [Elm Packages](https://package.elm-lang.org)
- [JSON tutorial by Elm Guide](https://elm-lang.org/docs/from-javascript)
- [JSON and Elm by Ellie](https://ellie-app.com/new)