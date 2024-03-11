---
date: 2024-01-20 17:48:16.208851-07:00
description: "Co i Dlaczego? Wyliczanie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w to\
  \ spos\xF3b, by dowiedzie\u0107 si\u0119, ile znak\xF3w zawiera dany tekst. Programi\u015B\
  ci robi\u0105 to, gdy potrzebuj\u0105 na\u2026"
lastmod: '2024-03-11T00:14:08.950132-06:00'
model: gpt-4-1106-preview
summary: "Co i Dlaczego? Wyliczanie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w to spos\xF3\
  b, by dowiedzie\u0107 si\u0119, ile znak\xF3w zawiera dany tekst. Programi\u015B\
  ci robi\u0105 to, gdy potrzebuj\u0105 na\u2026"
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
---

{{< edit_this_page >}}

## What & Why?
Co i Dlaczego?

Wyliczanie długości ciągu znaków to sposób, by dowiedzieć się, ile znaków zawiera dany tekst. Programiści robią to, gdy potrzebują na przykład walidować dane wejściowe lub manipulować tekstem.

## How to:
Jak to zrobić:

W Swift obliczenie długości stringa to bułka z masłem. Użyj właściwości `count` na instancji `String`. Oto jak to wygląda w kodzie:

```Swift
let greeting = "Dzień dobry!"
let length = greeting.count
print("Długość napisu: \(length)")
```

Wynik działania kodu:

```
Długość napisu: 12
```

Ale uwaga na Unicode! Policzmy znaki w emoji:

```Swift
let emoji = "👨‍👩‍👧‍👦"
let emojiLength = emoji.count
print("Długość emoji: \(emojiLength)")
```

Ile to jest?

```
Długość emoji: 1
```

Emoji rodzinne traktowane jest jako jeden znak, mimo że jest złożone z kilku innych.

## Deep Dive:
W Głąb Tematu:

W przeszłości liczenie znaków w stringu w Swift mogło być bardziej skomplikowane - używano indeksów, co było mniej przejrzyste. Dziś mamy `.count`, ale warto pamiętać, że Swift traktuje stringi jako kolekcje znaków Unicode, a nie proste tablice bajtów. To oznacza, że każdy grafem, czyli najmniejsza wizualna jednostka tekstu, liczy się jako jeden "znak" bez względu na liczbę składników Unicode, z której się składa.

Alternatywy? Można bawić się w niższopoziomowe manipulacje, ale po co, skoro `.count` robi to za nas efektywnie i bezpiecznie.

Szczegół implementacyjny: Swift używa czegoś zwanego "grapheme clustering", co jest zgodne ze standardem Unicode. Podczas przetwarzania tekstu trzeba pamiętać, że operacje na stringach mogą być kosztowne czasowo - zwłaszcza, gdy pracujemy z bardzo długimi ciągami znaków.

## See Also:
Zobacz Również:

- Dokumentacja Swift `String`: [Apple Developer Documentation](https://developer.apple.com/documentation/swift/string)
- Unicode i Swift String: [Swift String and Unicode](https://swift.org/blog/utf8-string/)
- Wykład o stringach w Swift: [Strings in Swift](https://academy.realm.io/posts/getting-to-know-swifts-string-type/)
