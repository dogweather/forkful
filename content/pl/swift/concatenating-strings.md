---
title:    "Swift: Łączenie ciągów znaków"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie stanowi nieodłączną część naszego życia. Dzięki niemu tworzymy aplikacje mobilne, strony internetowe, gry i wiele innych. W tym artykule skupimy się na jednym z podstawowych elementów języka Swift - łączeniu ciągów tekstowych (concatenating strings). Będziemy dokładniej przyglądać się dlaczego jest to ważne oraz jak to zrobić.

## Jak to zrobić

W Swift łączenie ciągów tekstowych jest bardzo proste i wykorzystuje operator "+" oraz metody "append()". Przede wszystkim musimy zdefiniować dwa ciągi tekstu, które chcemy połączyć. Następnie, używając operatora "+" możemy je połączyć w jeden ciąg, np:

```Swift
let imie = "Kasia"
let nazwisko = "Kowalska"
let pelneImie = imie + " " + nazwisko
```
W wyniku otrzymamy ciąg tekstu "Kasia Kowalska". Możemy również wykorzystać metodę "append()", która pozwala na dodawanie kolejnych fragmentów tekstu do już istniejącego ciągu. Przykład:

```Swift
var zwierzeta = "Koty"
zwierzeta.append(", psy, chomiki")
```

W rezultacie, zmienna "zwierzeta" będzie zawierać ciąg "Koty, psy, chomiki". Warto również wspomnieć o funkcji "join()", która pozwala na łączenie ciągów tekstowych z tablic lub kolekcji. Przykład:

```Swift
let lista = ["jabłka", "banany", "truskawki"]
let rozdzielacz = ", "
let listaOwocow = lista.joined(separator: rozdzielacz)
```

Wynikiem będzie ciąg "jabłka, banany, truskawki".

## Głębszy wgląd

Warto wiedzieć, że proces łączenia ciągów tekstowych może być kosztowny dla naszej aplikacji, szczególnie jeśli wykorzystujemy go w pętlach lub w skomplikowanych operacjach. W takich przypadkach zamiast łączyć ciągi z użyciem operatora "+", lepiej skorzystać z metody "append()". Co więcej, istnieje również inny operator - "&", który jest bardziej wydajny w łączeniu większej ilości ciągów.

## Zobacz również

- [Oficjalna dokumentacja Swift dotycząca łączenia ciągów tekstowych](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID285)
- [Codility: Concatenating Strings](https://codility.com/media/train/ConcatenatingStrings.pdf)
- [Ray Wenderlich: Swift Strings and Characters](https://www.raywenderlich.com/731-swift-strings-and-characters-cheat-sheet)