---
title:    "Swift: Wycinanie podciągów"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego warto wyodrębniać podłańcuchy?

Wyodrębnianie podłańcuchów jest nieodłączną częścią programowania w języku Swift, ponieważ pozwala nam na precyzyjne manipulowanie tekstem. Możesz wyodrębniać pojedyncze znaki, wyrazy lub nawet całe fragmenty tekstu w zależności od potrzeb. To nie tylko przydatne, ale także bardzo proste do zaimplementowania w Twoim kodzie.

## Jak to zrobić?

Aby wyodrębnić podłańcuchy w Swift, wystarczy użyć metody `substring` na przykładzie zmiennej `text`, która zawiera nasz tekst:

```Swift
let text = "Witaj w świecie Swift!"
let extractedSubstring = text.substring(from: 12)
```

W powyższym przykładzie wyodrębniamy podłańcuch zaczynający się od 12 znaku, czyli "Swift!". Zauważ, że liczymy znaki od 0, więc 12 odpowiada 13 znakowi w tekście. Istnieją również inne metody do wyodrębniania podłańcuchów, takie jak `substring(to:)` czy `substring(with:)`, ale te dwie są najbardziej popularne.

## Głębsza analiza

Wyodrębnianie podłańcuchów jest możliwe dzięki temu, że w języku Swift stosowany jest typ `String.UTF16View`, a nie tradycyjny `String`. Dzięki temu, każdy znak w łańcuchu jest traktowany jako 16-bitowa jednostka kodowa. Jest to szczególnie przydatne, jeśli pracujesz z językami, które używają innych znaków niż ASCII.

## Zobacz też

- [Dokumentacja Apple na temat wyodrębniania podłańcuchów w Swift](https://developer.apple.com/documentation/swift/string/1643045-substring)
- [Poradnik na temat manipulowania tekstami w języku Swift](https://www.ralfebert.de/ios/tutorials/stringswift/)
- [Przydatna porównywarka Swift String Manipulation](https://www.hackingwithswift.com/articles/112/7-useful-swift-string-examples)