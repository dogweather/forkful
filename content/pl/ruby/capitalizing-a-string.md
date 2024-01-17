---
title:                "Zmiana na wielkie litery ciągu znaków"
html_title:           "Ruby: Zmiana na wielkie litery ciągu znaków"
simple_title:         "Zmiana na wielkie litery ciągu znaków"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i czemu?

Niektórzy z was być może zastanawiają się, dlaczego programiści robią "cięcie" (ang. capitalizing) łańcucha znaków (ang. string) - co to jest i po co to robić? Otóż, "cięcie" to po prostu zamiana pierwszej litery łańcucha na dużą literę, a reszty na małe. Wygodne jest to na przykład przy wypisywaniu imion czy tytułów i dzięki temu tekst jest czytelniejszy. Ułatwia to również porównywanie łańcuchów znaków.

## Jak:

```ruby
string = "hello world"
capitalized = string.capitalize

puts capitalized # output: "Hello world"
```

W powyższym przykładzie widzimy, że używając metody "capitalize" dostępnej dla obiektów typu String, możemy w prosty sposób zmienić format tekstu. Warto również zauważyć, że ta metoda zwraca kopię łańcucha, a nie modyfikuje oryginalnego obiektu.

## Głębsze zanurzenie:

"Capitalizing" zostało wprowadzone w dawnych czasach, kiedy drukarki i komputery nie były w stanie wyświetlać dużych i małych liter jednocześnie. W dzisiejszych czasach można by uznać to za zbędność, ale nadal często spotyka się zastosowanie tej metody w różnych językach programowania.

Istnieją również inne sposoby zmiany formatu tekstu, np. "downcasing" (zmiana na małe litery) czy "upcasing" (zmiana na duze litery). W Ruby możemy również użyć metody "swapcase" aby zamienić litery na przeciwne - wielkie na małe i na odwrót.

Implementacja metody "capitalize" w Rubym jest dość prosta, ponieważ należy tylko zmienić pierwszą literę na dużą, a resztę na małe przy użyciu metody "downcase".

## Zobacz również:

Jeśli chcesz dowiedzieć się więcej na temat metod dla obiektów typu String w Ruby, koniecznie sprawdź dokumentację: https://ruby-doc.org/core-3.0.0/String.html 

Możesz również poznać inne ciekawe sposoby formatowania tekstu, np. "snake_case" czy "camelCase". Wszystko zależy od preferencji programisty i zastosowania w danym projekcie.