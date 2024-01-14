---
title:    "Elm: Przekształcanie ciągu znaków na małe litery"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

 Kiedy pracujesz z danymi tekstowymi w programowaniu, często musisz operować na różnych formach rodzajów liter. Jedną z najważniejszych funkcji jest konwersja całego tekstu na małe litery. Dzięki temu możesz łatwiej porównywać lub szukać dany wyraz w tekście. W Elm istnieje wbudowana funkcja, która umożliwia szybką konwersję tekstu na małe litery. W tym wpisie dowiesz się jak to zrobić.

## Jak to zrobić

Konwersja tekstu na małe litery w Elm jest bardzo prosta. Wykorzystujemy do tego funkcję `String.toLower`, która jest dostępna w standardowej bibliotece języka.

```Elm
import String

String.toLower "ELM Programowanie" -- wynik: "elm programowanie"
```

Jak widzisz, funkcja ta przyjmuje jako argument tekst i zwraca tekst w formacie wszystkich małych liter. Ważne jest również, aby zauważyć, że nie jesteśmy ograniczeni tylko do konwersji liter w języku angielskim. Funkcja ta będzie działać również dla tekstu w innych językach.

```Elm
String.toLower "Żółta Świeca" -- wynik: "żółta świeca"
```

## Głębszy zanurzenie

Aby lepiej zrozumieć jak funkcja `String.toLower` działa, warto zapoznać się z pojęciem kodowania znaków. W skrócie, każda litera ma swoją odpowiednią liczbę w tabeli znaków, a funkcja `String.toLower` przekształca te liczby na odpowiednie ciągi znaków reprezentujące małe litery. Dzięki temu możemy uzyskać spójność przy porównywaniu i przetwarzaniu tekstu.

Jednym z problemów, które może się pojawić, jest brak wsparcia dla specjalnych znaków lub znaków spoza standardowego kodowania. W takim przypadku można użyć funkcji `String.foldl` w celu zdefiniowania własnego algorytmu konwersji na potrzeby swojego projektu.

## Zobacz również

- [Oficjalna dokumentacja funkcji String.toLower w Elm](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [Przykładowe projekty w Elm na stronie webowej GitHub](https://github.com/topics/elm)
- [Kurs programowania w języku Elm na platformie Udemy](https://www.udemy.com/course/elm-programming/)