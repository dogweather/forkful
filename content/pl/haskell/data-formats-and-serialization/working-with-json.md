---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:47.465212-07:00
description: "Praca z JSON (JavaScript Object Notation) w Haskellu polega na parsowaniu\
  \ danych JSON do typ\xF3w Haskell oraz konwertowaniu typ\xF3w Haskell z powrotem\
  \ na\u2026"
lastmod: '2024-03-13T22:44:35.475047-06:00'
model: gpt-4-0125-preview
summary: "Praca z JSON (JavaScript Object Notation) w Haskellu polega na parsowaniu\
  \ danych JSON do typ\xF3w Haskell oraz konwertowaniu typ\xF3w Haskell z powrotem\
  \ na\u2026"
title: Praca z JSON
weight: 38
---

## Co i dlaczego?
Praca z JSON (JavaScript Object Notation) w Haskellu polega na parsowaniu danych JSON do typów Haskell oraz konwertowaniu typów Haskell z powrotem na JSON. Programiści robią to, aby umożliwić swoim aplikacjom Haskell wymianę danych z usługami sieciowymi lub API w sposób płynny, co jest powszechną praktyką w nowoczesnym rozwoju oprogramowania dla międzyplatformowej wymiany danych.

## Jak:
Haskell nie ma wbudowanego wsparcia dla JSON, tak jak JavaScript, ale z pomocą bibliotek stron trzecich, takich jak **Aeson**, obsługa JSON staje się prosta. Aeson oferuje zarówno funkcje wysokiego, jak i niskiego poziomu do kodowania (konwertowanie wartości Haskell na JSON) oraz dekodowania (parsowanie JSON do wartości Haskell).

### Instalacja Aeson
Najpierw dodaj Aeson do zależności swojego projektu, aktualizując plik `.cabal` lub bezpośrednio używając Stack lub Cabal:

```shell
cabal update && cabal install aeson
```
lub, jeśli używasz Stack:
```shell
stack install aeson
```

### Parsowanie JSON
Zacznijmy od podstawowego przykładu dekodowania danych JSON na typ Haskell. Załóżmy, że mamy następujący JSON reprezentujący osobę:

```json
{
  "name": "John Doe",
  "age": 30
}
```

Najpierw zdefiniuj odpowiadający mu typ danych Haskell i uczyn go instancją `FromJSON`:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Generic, Show)

instance FromJSON Person

-- Funkcja do dekodowania JSON z pliku
decodePerson :: FilePath -> IO (Maybe Person)
decodePerson filePath = do
  personJson <- B.readFile filePath
  return $ decode personJson
```
Sposób użycia:
Zakładając, że `person.json` zawiera powyższe dane JSON, uruchom:
```haskell
main :: IO ()
main = do
  maybePerson <- decodePerson "person.json"
  print maybePerson
```
Przykładowy wynik:
```haskell
Just (Person {name = "John Doe", age = 30})
```

### Kodowanie wartości Haskell jako JSON
Aby przekonwertować wartość Haskell z powrotem na JSON, musisz uczynić swój typ instancją `ToJSON`, a następnie użyć `encode`.

```haskell
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- Zakładając typ Person z poprzednich przykładów

instance ToJSON Person

encodePerson :: Person -> B.ByteString
encodePerson = encode

main :: IO ()
main = do
  let person = Person "Jane Doe" 32
  putStrLn $ show $ encodePerson person
```
Przykładowy wynik:
```json
{"name":"Jane Doe","age":32}
```

Te przykłady demonstrują podstawy pracy z JSON w Haskellu, używając Aeson. Pamiętaj, że Aeson oferuje znacznie więcej, w tym niestandardowe reguły parsowania, pracę z zagnieżdżonym JSONem i wiele innych, odpowiednie dla różnych potrzeb i scenariuszy.
