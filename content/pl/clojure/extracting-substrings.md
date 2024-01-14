---
title:    "Clojure: Wyodrębnianie podciągów"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Dlaczego

Często podczas programowania w Clojure musimy pracować z ciągami znaków, które są długie i skomplikowane. Czasami potrzebujemy tylko części tych ciągów, ale ręcznie wybieranie odpowiednich fragmentów może być uciążliwe i czasochłonne. Dlatego warto poznać narzędzie, które pozwoli nam w prosty sposób wyciąć potrzebne nam podciągi.

## Jak to zrobić

W Clojure mamy dostęp do funkcji `subs`, która umożliwia nam wycinanie podciągów z dowolnego ciągu znaków. Przykładowo, jeżeli chcemy uzyskać podciąg zaczynający się od drugiego znaku a kończący na siódmym, wykonujemy następującą operację:

```Clojure
(subs "Lorem ipsum dolor sit amet" 2 7)
```
Wynik: `orem `

Jeżeli chcemy wybrać tylko fragment od konkretnej pozycji do końca ciągu, możemy zastosować następujący zapis:
```Clojure
(subs "Lorem ipsum dolor sit amet" 12)
```
Wynik: `sit amet`

Funkcja `subs` przyjmuje jako argumenty ciąg znaków oraz pozycje startową i opcjonalnie końcową do wycinania.

Możliwe jest również wycinanie podciągów od końca ciągu, co jest szczególnie przydatne przy pracy z ciągami znaków o zmiennej długości. W takim przypadku pozycje należy podawać ujemne, np. `-5` oznacza piąty znak od końca. Przykład:

```Clojure
(subs "Lorem ipsum dolor sit amet" -9 -3)
```

Wynik: ` dolor `

## Wgląd w działanie funkcji `subs`

Warto wiedzieć, że funkcja `subs` w rzeczywistości korzysta z jednej z innych funkcji dostępnych w Clojure - `substring`. Możemy to wykorzystać, jeżeli chcemy uzyskać więcej szczegółów o funkcjonalności wyciągania podciągów.

## Zobacz również

- Dokumentacja funkcji `subs`: https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/subs
- Więcej informacji o funkcji `substring` i sposobach jej użycia: https://clojuredocs.org/clojure.java.string/substring