---
title:                "Wydobywanie podciągów"
html_title:           "Swift: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

W Swift'cie, "wycinanie podciągów" to nic innego jak wydobycie części napisu z innego napisu. Programiści często to robią, aby manipulować tekstem lub otrzymać konkretny fragment danych.

## Jak: 

### Przykład 1: Wybieranie podciągu ze stałej wartości

```Swift
let napis = "Witaj, Świecie!"
let podciag = String(napis.suffix(10))
print(podciag)

// Output: Świecie!
```

### Przykład 2: Wybieranie podciągu ze zmiennej
```Swift
let nazwaMiasta = "Warszawa"
let poczatek = nazwaMiasta.startIndex
let koniec = nazwaMiasta.index(nazwaMiasta.startIndex, offsetBy: 3)
let podciag = nazwaMiasta[poczatek...koniec]
print(podciag)

// Output: War
```

## Głębszy zanurzenie:

### Kontekst historyczny:

Wyciąganie podciągów było stosowane od dawna w językach programowania, takich jak C i C++. Jednak w Swift'cie dostępne są nowe metody, takie jak `prefix()` i `suffix()`, które ułatwiają to zadanie.

### Alternatywy:

Alternatywą dla wycinania podciągów może być użycie metody `subString()` lub `substring(with:)`.

### Szczegóły implementacji:

Do wycinania podciągów można użyć kilku różnych metod, takich jak `prefix()`, `suffix()`, `subString()` lub `substring(with:)`. Każda z tych metod akceptuje różne argumenty, na przykład: indeksy, zakresy lub predykaty.

## Zobacz też:

- [Swift String and Character documentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift 4 strings and substrings explained (with code examples)](https://www.appcoda.com/swift-4-strings/)