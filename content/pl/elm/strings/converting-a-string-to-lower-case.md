---
date: 2024-01-20 17:38:07.856487-07:00
description: "Konwersja tekstu do ma\u0142ych liter polega na zmianie wszystkich wielkich\
  \ liter w ci\u0105gu znak\xF3w na ich ma\u0142e odpowiedniki. Programi\u015Bci u\u017C\
  ywaj\u0105 tej operacji, by\u2026"
lastmod: '2024-03-11T00:14:08.486286-06:00'
model: gpt-4-1106-preview
summary: "Konwersja tekstu do ma\u0142ych liter polega na zmianie wszystkich wielkich\
  \ liter w ci\u0105gu znak\xF3w na ich ma\u0142e odpowiedniki. Programi\u015Bci u\u017C\
  ywaj\u0105 tej operacji, by\u2026"
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Konwersja tekstu do małych liter polega na zmianie wszystkich wielkich liter w ciągu znaków na ich małe odpowiedniki. Programiści używają tej operacji, by ujednolicić dane, szczególnie w przypadku porównań, wyszukiwania i walidacji.

## How to: (Jak to zrobić:)
Elm używa funkcji `String.toLower` do konwersji tekstów. Oto przykład z wykorzystaniem:

```Elm
import String

lowercaseString : String -> String
lowercaseString str =
    String.toLower str

-- Użycie funkcji:
result = lowercaseString "HeLLO, WoRLd!"

-- Wynik:
-- "hello, world!"
```

## Deep Dive (Dogłębna analiza)
Konwersja na małe litery istnieje od dawna w programowaniu. W Elm, standardowa biblioteka obsługuje tę operację bezpośrednio za pomocą funkcji `String.toLower`. Alternatywą może być własna implementacja przez mapowanie każdego znaku z użyciem funkcji `Char.toLower`. Wynika to z potrzeby obsługi wielu języków i różnych zestawów znaków, gdzie konwersja nie zawsze jest trywialna i może wymagać dodatkowej logiki (np. w językach z alfabetami innych niż łacińskie).

Implementacja `String.toLower` w Elm stosuje reguły Unicode dla przekształceń znaków, dzięki czemu działa poprawnie w większości przypadków przy użyciu różnorodnych zestawów znaków.

## See Also (Zobacz też)
- Dokumentacja Elm dla `String.toLower`: https://package.elm-lang.org/packages/elm/core/latest/String#toLower
- Unicode case mapping: https://www.unicode.org/reports/tr21/tr21-5.html
- Artykuł o normalizacji danych: https://en.wikipedia.org/wiki/Unicode_equivalence
