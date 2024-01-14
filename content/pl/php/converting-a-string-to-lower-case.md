---
title:    "PHP: Konwertowanie ciągu znaków na małe litery"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że podczas pisania kodu w PHP potrzebujemy zmienić wszystkie litery w tekście na małe. Może to być przydatne przy walidacji danych użytkownika lub w przypadku konieczności porównania tekstu bez uwzględniania wielkości liter. W tym wpisie dowiesz się, jak łatwo dokonać takiej operacji w PHP.

## Jak to zrobić?

```PHP
$string = "WIELKIE LITERY";
echo strtolower($string);
```

Wynik: `wielkie litery`

W powyższym przykładzie wykorzystujemy funkcję `strtolower()` wbudowaną w PHP, która automatycznie konwertuje wszystkie litery w podanym tekście na małe. Dodatkowo, możemy również wykorzystać funkcję `mb_strtolower()`, która pozwala na konwersję znaków w wielu językach.

## Głębszy zanurzenie

Podczas wykorzystywania funkcji `strtolower()` warto mieć na uwadze różnice między wielkościami liter w różnych językach. Na przykład, w języku angielskim `i` po zamianie na małe litery staje się `I`, jednak w języku polskim `I` po zamianie na małe litery staje się `i`. W przypadku, gdy kodujemy dla wielojęzycznej strony, warto wykorzystywać funkcję `mb_strtolower()` i określać w niej odpowiednie ustawienia dla konkretnych języków.

## Zobacz także

- [Oficjalna dokumentacja PHP o funkcji strtolower()](https://www.php.net/manual/en/function.strtolower.php)
- [Podstawy kodowania w PHP - wideo tutorial](https://www.youtube.com/watch?v=bUCaVBO-2ms)