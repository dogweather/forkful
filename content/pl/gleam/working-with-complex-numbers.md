---
title:                "Praca z liczbami zespolonymi"
date:                  2024-01-26T04:40:37.026842-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z liczbami zespolonymi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Liczby zespolone mają część rzeczywistą i część urojoną (`a + bi`). Są przydatne w różnych dziedzinach, takich jak elektrotechnika i kwantowe obliczenia. Programiści używają ich do modelowania równań, których nie można rozwiązać, używając tylko liczb rzeczywistych.

## Jak to zrobić:
Gleam nie ma wbudowanego wsparcia dla liczb zespolonych. Zazwyczaj musisz stworzyć własną implementację lub znaleźć bibliotekę. Oto krótki przykład, jak można zaimplementować podstawowe operacje:

```gleam
type Complex {
  Complex(Float, Float)
}

fn add(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a + x, b + y)
}

fn multiply(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a*x - b*y, a*y + b*x)
}

fn main() {
  let num1 = Complex(1.0, 2.0)
  let num2 = Complex(3.0, 4.0)
  let suma = add(num1, num2)
  let iloczyn = multiply(num1, num2)

  suma // Complex(4.0, 6.0)
  iloczyn // Complex(-5.0, 10.0)
}
```

## Pogłębiona analiza

Liczby zespolone zostały po raz pierwszy bardziej formalnie udokumentowane przez Gerolamo Cardano w XVI wieku. Są naturalnym rozszerzeniem liczb rzeczywistych. Jednak w młodym języku, jakim jest Gleam, który priorytetowo traktuje wydajność i bezpieczeństwo typów, takie funkcje są na poziomie podstawowym (lub musisz zrobić to sam).

W niektórych innych językach, takich jak Python, liczby zespolone są wbudowane (`3+4j`), co ułatwia życie. W Rust czy Haskell istnieją biblioteki, które oferują zaawansowane funkcjonalności „od razu”.

Podejście w Gleam oznacza, że musisz samodzielnie obsługiwać wszystkie aspekty: arytmetykę, współrzędne biegunowe, formy wykładnicze itp. Implementacja efektywnych, dokładnych operacji wymaga starannego programowania, biorąc pod uwagę, jak zachowanie liczb zmiennoprzecinkowych może wpływać na twoje wyniki.

Pamiętaj o dokładnym testowaniu, szczególnie przypadków skrajnych! Obsługa nieskończoności zespolonej i wartości NaN (nie liczba) może sprawić ci kłopot, jeśli nie będziesz ostrożny.

## Zobacz również
Aby zdobyć więcej ciekawostek, oto, gdzie możesz się zagłębić:
- [Oficjalna dokumentacja Gleam](https://gleam.run/documentation/)
- Zainspiruj się bibliotekami innych języków, takimi jak [num-complex](https://crates.io/crates/num-complex) Rusta czy moduł [cmath](https://docs.python.org/3/library/cmath.html) Pythona.