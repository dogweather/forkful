---
title:                "Zmiana wielkości liter w ciągu znaków"
html_title:           "Ruby: Zmiana wielkości liter w ciągu znaków"
simple_title:         "Zmiana wielkości liter w ciągu znaków"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego warto skorzystać z opcji kapitalizacji w programowaniu w Ruby? Głównym powodem jest możliwość zmiany wyglądu ciągu znaków w spójny i czytelny sposób. Może to być szczególnie przydatne, gdy pracujemy z danymi użytkowników lub formatujemy wyświetlane informacje.

## Jak to zrobić

Podczas programowania w Ruby, istnieje kilka sposobów na kapitalizację ciągów znaków. Jednym z najprostszych sposobów jest użycie metody `capitalize`, która zwraca kopię ciągu znaków z pierwszą literą zmienioną na wielką. Możemy to osiągnąć w następujący sposób:

```Ruby
text = "hello world"
puts text.capitalize
```

Wynik:

```Ruby
Hello world
```

Inną metodą jest użycie metody `upcase`, która zwraca kopię ciągu znaków z wszystkimi literami zmienionymi na wielkie. Przykład:

```Ruby
text = "hello world"
puts text.upcase
```

Wynik:

```Ruby
HELLO WORLD
```

## Głębsza analiza

Warto zauważyć, że metody `capitalize` i `upcase` nie zmieniają oryginalnego ciągu znaków, ale zwracają jego zmienioną kopię. Istnieje również możliwość zmiany oryginalnego ciągu znaków poprzez użycie wykrzyknika `!` na końcu metody, np. `text.upcase!`. W ten sposób zmieniamy wartość zmiennej `text` na oryginalną. 

Ponadto, jeśli chcemy zmienić tylko pierwszą literę na wielką, ale zachować pozostałe w oryginalnej formie, możemy użyć metody `capitalize!`, która zmieni tylko pierwszą literę w zmiennej `text` bez zmiany pozostałych. 

## Zobacz także

- [Dokumentacja Ruby - metoda capitalize](https://ruby-doc.org/core-2.7.1/String.html#method-i-capitalize)
- [Dokumentacja Ruby - metoda upcase](https://ruby-doc.org/core-2.7.1/String.html#method-i-upcase)