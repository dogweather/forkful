---
changelog:
- 2024-01-30, dogweather, reviewed
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:33.533292-07:00
description: "Jak to zrobi\u0107: Haskell nie posiada wbudowanych tablic asocjacyjnych\
  \ w taki sam spos\xF3b jak niekt\xF3re inne j\u0119zyki, ale oferuje pot\u0119\u017C\
  n\u0105 bibliotek\u0119 standardow\u0105\u2026"
lastmod: '2024-03-13T22:44:35.444308-06:00'
model: gpt-4-0125-preview
summary: "Haskell nie posiada wbudowanych tablic asocjacyjnych w taki sam spos\xF3\
  b jak niekt\xF3re inne j\u0119zyki, ale oferuje pot\u0119\u017Cn\u0105 bibliotek\u0119\
  \ standardow\u0105 o nazwie `Data.Map` do pracy z parami klucz-warto\u015B\u0107\
  ."
title: Korzystanie z tablic asocjacyjnych
weight: 15
---

## Jak to zrobić:
Haskell nie posiada wbudowanych tablic asocjacyjnych w taki sam sposób jak niektóre inne języki, ale oferuje potężną bibliotekę standardową o nazwie `Data.Map` do pracy z parami klucz-wartość. Zwinmy rękawy i zobaczmy, jak ich używać!

Najpierw upewnij się, że zaimportowałeś ją:
```Haskell
import qualified Data.Map as Map
```

Tworzenie mapy jest proste. Stwórzmy jedną z językami programowania i ich paradygmatami:
```Haskell
let languages = Map.fromList [("Haskell", "Funkcyjny"), ("Python", "Imperatywny"), ("Prolog", "Logiczny")]
```

A co, jeśli chcemy uzyskać paradygmat Haskell?
```Haskell
Map.lookup "Haskell" languages
-- wynik: Just "Funkcyjny"
```

Dodanie nowego języka jest łatwe:
```Haskell
let languagesUpdated = Map.insert "Rust" "Systemowy" languages
```

Co jeśli chcemy wymienić wszystkie języki? Użyj `Map.keys`:
```Haskell
Map.keys languagesUpdated
-- wynik: ["Haskell","Python","Prolog","Rust"]
```

Aby wymienić paradygmaty, użyj `Map.elems`:
```Haskell
Map.elems languagesUpdated
-- wynik: ["Funkcyjny","Imperatywny","Logiczny","Systemowy"]
```

Te podstawowe operacje powinny pokryć większość zastosowań, ale jest jeszcze wiele do odkrycia w `Data.Map`!

## Pogłębiona analiza
Moduł `Data.Map` w standardowej bibliotece Haskell jest zbudowany na podstawie zrównoważonych drzew binarnych, konkretnie na drzewach AVL. Ten wybór zapewnia, że większość operacji na mapie, takich jak wstawianie, usuwanie i wyszukiwanie, może być wykonywana w czasie O(log n), gdzie n to liczba elementów w mapie. Jest to efektywny wybór dla wielu przypadków użycia, chociaż nie zawsze najszybszy we wszystkich scenariuszach.

Jest też pewien historyczny niuans: zanim `Data.Map` stał się narzędziem wyboru, programiści Haskell często używali list par do symulowania tablic asocjacyjnych. Jednak operacje na takich strukturach są O(n) dla wyszukiwania, co czyni `Data.Map` znaczącą poprawę pod względem wydajności.

Teraz, pomimo wydajności i użyteczności `Data.Map`, nie zawsze jest to najlepsze narzędzie do każdego zadania. W przypadkach wysoce wrażliwych na wydajność, gdzie nawet czas wyszukiwania O(log n) jest zbyt wolny, lub gdy klucze są zawsze wartościami całkowitymi, tablice lub tabele mieszające (poprzez `Data.HashMap`) mogą oferować lepszą wydajność z czasami dostępu O(1).

Ekosystem Haskell pozwala na różnorodność struktur danych, dostosowanych do różnych potrzeb, a `Data.Map` jest doskonałym wyborem ogólnego zastosowania dla tablic asocjacyjnych, łącząc łatwość użycia, elastyczność i wydajność.
