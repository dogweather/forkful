---
title:    "Haskell: Wyodrębnianie podłańcuchów"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

W dziennym programowaniu często musimy pracować z tekstowymi danymi i czasami potrzebujemy wyodrębnić pewne części tych danych. W tym wpisie na blogu dowiesz się, jak w języku Haskell wyodrębniać podłańcuchy, aby ułatwić sobie pracę z tekstami.

## Jak to zrobić

Wyodrębnienie podłańcucha w Haskellu jest bardzo proste i wygodne dzięki wbudowanej funkcji "take" oraz "drop". Oto kilka przykładów kodu z użyciem tych funkcji:

```Haskell
-- Utworzenie przykładowego tekstu
tekst = "To jest przykładowy tekst"

-- Pobranie pierwszych 10 znaków
take 10 tekst
-- Wynik: "To jest pr"

-- Pominięcie pierwszych 5 znaków
drop 5 tekst
-- Wynik: " jest przykładowy tekst"
```

Możemy także wykorzystać funkcję "takeWhile" i "dropWhile" do wyodrębnienia podłańcucha spełniającego określone warunki. Przykłady użycia tych funkcji:

```Haskell
-- Utworzenie przykładowej listy liczb
liczby = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

-- Pobranie liczb mniejszych niż 5
takeWhile (<5) liczby
-- Wynik: [1, 2, 3, 4]

-- Pominięcie liczb większych niż 7
dropWhile (>7) liczby
-- Wynik: [1, 2, 3, 4, 5, 6, 7]
```

## Głębszy zanurzenie

Istnieją również inne sposoby na wyodrębnienie podłańcucha w Haskellu, takie jak korzystanie z funkcji "substring" z modułu Data.Text lub wykorzystanie wyrażeń regularnych z pomocą modułu Text.Regex. Dokładniejsze informacje na ten temat można znaleźć w dokumentacji języka Haskell.

## Zobacz także

Jeśli jesteś zainteresowany/a nauką języka Haskell i programowaniem funkcyjnym, polecam zapoznać się z następującymi linkami:

- [Oficjalna dokumentacja języka Haskell](https://www.haskell.org/documentation/)
- [Kurs programowania funkcyjnego w Haskellu](https://www.seas.upenn.edu/~cis194/spring13/)
- [Nauka programowania funkcyjnego w języku Haskell z projektami](https://www.codewars.com/collections/fun-with-func-templates-courses-middle-level)