---
title:                "PHP: Znajdowanie długości ciągu znaków"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego

W dzisiejszym poście dowiesz się, dlaczego jest istotne znaleźć długość ciągu znaków w PHP i jak to zrobić.

# Jak to zrobić

Aby znaleźć długość ciągu znaków w PHP, można użyć wbudowanej funkcji `strlen()`. Przykładowy kod wygląda następująco:

```PHP
$string = "Witaj, świecie!";
echo strlen($string);
```

Po uruchomieniu tego kodu, ujrzymy na ekranie liczbę `16`, która jest długością ciągu `Witaj, świecie!`.

# Deep Dive

Funkcja `strlen()` liczy liczbę znaków w danym ciągu, włącznie ze spacjami. Dlatego warto zwrócić uwagę na używane znaki, ponieważ mogą one mieć wpływ na wynik. Jeśli chcemy policzyć tylko znaki alfanumeryczne, można użyć funkcji `preg_replace()` w celu usunięcia innych znaków.

# Zobacz także

- Dokumentacja PHP: https://www.php.net/manual/en/function.strlen.php
- Wideo tutorial na temat funkcji `strlen()`: https://www.youtube.com/watch?v=Pz8LSvgsNbg
- Przykładowe zadanie dla praktyki: https://www.w3resource.com/php-exercises/php-basic-exercise-3.php