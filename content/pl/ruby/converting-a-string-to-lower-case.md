---
title:                "Ruby: Konwertowanie ciągu znaków na małe litery."
simple_title:         "Konwertowanie ciągu znaków na małe litery."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

W tym wpisie chciałbym przybliżyć Wam temat konwersji ciągu znaków na małe litery w języku Ruby. Dowiecie się, dlaczego jest to przydatne i jak to zrobić przy użyciu kilku prostych linii kodu.

## Jak to zrobić

W celu przekształcenia ciągu znaków na małe litery, używamy metody `downcase`. Możemy ją wywołać na dowolnym stringu i zwróci ona nowy string z wszystkimi literami w małym formacie. Przykładowy kod wyglądałby następująco:

```Ruby
str = "Hello World!"
puts str.downcase
```

Output: `hello world!`

Możemy również wywołać tę metodę na stałej, na przykład:

```Ruby
CONSTANT = "UPPERCASE"
puts CONSTANT.downcase
```

Output: `uppercase`

Warto również wiedzieć, że ta metoda nie tylko konwertuje duże litery na małe, ale także pozostawi niezmienione znaki specjalne oraz numeryczne. Dzięki temu nie musimy martwić się o przypadkowe usunięcie lub zmianę innych znaków niż litery.

## Głębszy wgląd

Metoda `downcase` używana jest głównie do porównywania stringów bez uwzględniania wielkości liter. Dzięki temu mamy pewność, że nawet jeśli użytkownik wpisze słowo z małych liter, a my oczekujemy wielkich, nasz program i tak wykona poprawne porównanie.

Jednakże, warto zaznaczyć, że ta metoda jest zależna od aktualnego ustawienia lokalizacji językowej. Dlatego, jeśli chcemy mieć pewność, że wszystkie litery zostaną przekształcone na małe, możemy użyć metody `unicode_normalize` wraz z `downcase`. Oto przykładowy kod:

```Ruby
str = "ŁĄKA"
puts str.unicode_normalize(:nfkc).downcase
```

Output: `łąka`

## Zobacz również

1. [Dokumentacja Ruby o metodzie downcase](https://ruby-doc.org/core-2.5.0/String.html#method-i-downcase)
2. [Porównywanie Stringów w języku Ruby](https://devstyle.pl/2011/02/26/porownywanie-stringow-w-ruby/) (po polsku)
3. [Metody stringów w języku Ruby](https://www.tutorialspoint.com/ruby/ruby_strings.htm) (po angielsku)