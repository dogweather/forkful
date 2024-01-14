---
title:                "Swift: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Dlaczego warto pisać do standardowego błędu 

Pisanie do standardowego błędu jest jedną z ważnych umiejętności programistycznych, ponieważ może pomóc nam w debugowaniu naszych programów. Gdy skrypt lub aplikacja zwraca nam wiele informacji, niektóre z nich mogą być sygnalizowane jako błąd. Wtedy pisanie do standardowego błędu pozwala na lepsze zrozumienie, co i gdzie wystąpił błąd.

# Jak to zrobić 

```Swift 
let number = 0 

if number == 0 {
  print("Pierwsza liczba jest równa 0.")
  fatalError("Nie możesz dzielić przez 0.")
}

print("Druga liczba jest równa 5.")
```

W powyższym przykładzie, gdy zmienna "number" ma wartość 0, zostanie wyświetlony komunikat "Nie możesz dzielić przez 0." przy użyciu funkcji fatalError(). Ten komunikat zostanie zapisany do standardowego błędu, dzięki czemu będzie łatwiej nam zlokalizować i naprawić błąd w naszym kodzie.

# Głębszy wgląd 

Pisanie do standardowego błędu jest również przydatne, gdy chcemy zrobić diagnozę problemu w naszym kodzie. Możemy wtedy wyświetlić różne komunikaty w zależności od rodzaju błędu, co pozwoli nam szybciej zidentyfikować jego przyczynę. 

# Zobacz też

- [Swift Standard Library](https://developer.apple.com/documentation/swift)
- [Debugowanie w Swift](https://docs.swift.org/swift-book/LanguageGuide/Patterns.html)