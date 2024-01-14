---
title:                "Swift: Ekstrakcja podciągów"
simple_title:         "Ekstrakcja podciągów"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Wyodrębnianie podciągów to ważne narzędzie w każdym języku programowania, a w Swift jest równie przydatne. Pozwala na wyodrębnienie części tekstu z danego ciągu znaków. Jest to szczególnie przydatne w manipulacji i analizie tekstów, takich jak analiza danych lub tworzenie złożonych wyrażeń.

## Jak to zrobić

Aby wyodrębnić podciąg w Swift, należy użyć metody "substring (from:)" dla typu String. W nawiasie należy podać indeks początkowy, od którego chcemy wyodrębnić podciąg.

```Swift
let text = "Witaj w świecie Swift!"

let substring = text.substring(from: 12)

print(substring)
```

Output: "świecie Swift!"

Możemy również wybrać tylko określoną ilość znaków z podciągu, używając metody "substring (to:)".

```Swift
let text = "Dzisiaj jest piękny dzień"

let substring = text.substring(to: 15)

print(substring)
```

Output: "Dzisiaj jest pię"

## Deep Dive

W przypadku wyodrębniania podciągów w Swift, istnieje kilka ważnych szczegółów, które warto wziąć pod uwagę. Po pierwsze, indeksy podciągów są liczone od zera, co oznacza, że pierwszy znak będzie miał indeks 0. Po drugie, można również użyć metody "substring (with:)" i podać zakres indeksów, zamiast pojedynczego indeksu początkowego lub końcowego.

```Swift
let text = "To jest przykładowy tekst"

let substring = text.substring(with: 8...15)

print(substring)
```

Output: "przykładowy "

Warto również zauważyć, że wyodrębniony podciąg będzie miał ten sam typ danych co oryginalny ciąg, co oznacza, że można na nim wykonywać inne operacje, takie jak sprawdzanie długości lub wykorzystywanie w innych funkcjach.

## Zobacz także:

- Dokumentacja Swift: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html
- Wykorzystywanie metod "substring" w praktyce: https://medium.com/@oscarjbarnes/writing-natural-conversations-with-swift-part-2-hello-and-goodbye-e88ecc4b074
- Często zadawane pytania dotyczące wyodrębniania podciągów w Swift: https://stackoverflow.com/questions/35110752/substring-in-swift-not-working