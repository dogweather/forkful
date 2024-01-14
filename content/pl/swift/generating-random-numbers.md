---
title:                "Swift: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Dlaczego generowanie losowych liczb jest ważne dla programistów
Generowanie losowych liczb jest częstym zadaniem dla programistów, ponieważ pozwala na tworzenie różnorodnych wariantów w swoich programach, co może być przydatne w różnych przypadkach. Na przykład, generowanie losowych liczb może pomóc w losowym wyborze elementów z listy lub symulacji różnych scenariuszy.

## Jak to zrobić
Aby wygenerować losową liczbę w języku Swift, należy użyć wbudowanej funkcji `random`. Przykładowy kod niżej pokazuje, jak generować losowe liczby z zakresu od 1 do 10.

```Swift
let randomNum = Int.random(in: 1...10)
print(randomNum)
```

To wydrukuje losową liczbę z zakresu od 1 do 10, na przykład `7`.

Jeśli chcemy wygenerować więcej niż jedną liczbę, możemy użyć pętli `for`, jak pokazano w przykładzie poniżej:

```Swift
for _ in 1...5 {
    let randomNum = Int.random(in: 1...10)
    print(randomNum)
}
```

Ten kod wygeneruje 5 liczb z zakresu od 1 do 10.

## Głębsze zagadnienia
W języku programowania, generowanie liczb z zakresu od 0 do 1 jest uważane za najbardziej losowe. W Swift, możemy użyć funkcji `Double.random(in: 0...1)` aby wygenerować taką liczbę. Istnieje również możliwość wygenerowania losowych liczb zmiennoprzecinkowych, używając funkcji `Double` lub `Float` zamiast `Int`.

Jedną z metod generowania liczb losowych jest wykorzystanie tzw. "ziarna" (ang. seed), które jest wartością początkową dla generatora liczb pseudolosowych. Domyślnie, w języku Swift, seed jest ustawiony na aktualny czas, co powoduje, że generowane liczby są zawsze inne przy każdym uruchomieniu programu. Jednak, jeśli chcemy osiągnąć dokładnie takie same wyniki, możemy ustawić seed ręcznie, używając funkcji `RandomNumberGenerator`.

## Zobacz również
- [Oficjalna dokumentacja Swift na temat generowania liczb losowych](https://developer.apple.com/documentation/swift/random)
- [Poradnik na temat generowania liczb losowych w języku Swift](https://www.hackingwithswift.com/example-code/language/how-to-generate-random-numbers-in-swift)