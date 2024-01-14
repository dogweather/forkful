---
title:                "Ruby: Zmiana wielkości liter w ciągu znaków"
simple_title:         "Zmiana wielkości liter w ciągu znaków"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego w programowaniu często spotykamy się z manipulacją tekstem? Jednym z najczęstszych zadań jest kapitalizowanie ciągów znaków - czyli zamiana pierwszej litery każdego wyrazu na wielką. Jest to przydatne w wielu sytuacjach, na przykład w wizualnym ulepszaniu wyświetlanych danych lub w tworzeniu nazw użytkowników.

## Jak to zrobić

```Ruby
puts "witaj świecie".capitalize
```

To jest prosty przykład użycia metody `capitalize`. Wykorzystujemy ją do zamiany pierwszej litery tekstu na wielką. Wynikiem tego kodu jest "Witaj świecie".

```Ruby
puts "hello world".titleize
```

Kolejna przydatna metoda to `titleize`, która zwraca tekst z kapitalizowanymi pierwszymi literami każdego wyrazu. Wynikiem powyższego kodu będzie "Hello World".

## Głębsza analiza

W Ruby możemy użyć również metody `upcase` oraz `downcase` w celu zamiany wszystkich liter tekstu na duże lub małe. Przykładowo:

```Ruby
puts "programowanie jest super".upcase
```

Wynikiem powyższego kodu będzie "PROGRAMOWANIE JEST SUPER". Natomiast metoda `downcase` zamienia wszystkie litery na małe.

```Ruby
puts "PROGRAMOWANIE JEST SUPER".downcase
```

Wynik to "programowanie jest super". Warto jednak zauważyć, że metody `upcase` i `downcase` mogą mieć inne wyniki w zależności od języka, w którym zostaną wywołane.

## Zobacz także

- [Metody do manipulacji stringami w Ruby](https://ruby-doc.org/core-3.0.0/String.html)
- [Przewodnik po Ruby dla początkujących](https://www.ruby-lang.org/pl/documentation/quickstart/)