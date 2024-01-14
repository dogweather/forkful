---
title:    "PHP: Wydrukowanie danych debugowania"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/php/printing-debug-output.md"
---

{{< edit_this_page >}}

# Dlaczego drukowanie danych debugujących jest ważne?

Debugowanie jest nieodłączną częścią procesu programowania. Jest to proces znajdowania i naprawiania błędów w kodzie, co jest niezbędne dla poprawnego działania aplikacji. Drukowanie danych debugujących jest jedną z metod, które pomagają programistom w identyfikowaniu problemów w kodzie. Pozwala to na wgląd w zmienną, wartości i kolejność wykonywania kodu, co jest niezbędne do znalezienia błędu i jego naprawy.

# Jak wykorzystać drukowanie danych debugujących w PHP?

Aby wyświetlić dane debugujące w PHP, należy użyć funkcji `print_r()` lub `var_dump()`. Poniżej przedstawione są przykłady kodów oraz wyników wyświetlanych na ekranie.

```PHP
<?php
$name = "Jan";
$age = 30;
$height = 175;
```

```PHP
print_r($name);
```
Output:
```
Jan
```

```PHP
var_dump($age);
```
Output:
```
int(30)
```

# Głębsze spojrzenie na drukowanie danych debugujących w PHP

Funkcja `print_r()` wyświetla zawartość zmiennej w czytelnej formie, co ułatwia identyfikację problemu. Natomiast `var_dump()` dodatkowo wyświetla informacje o typie i długości zmiennej. Dzięki temu, możemy błyskawicznie zauważyć, jeśli zmienna jest niezdefiniowana lub ma nieprawidłową wartość.

Innym przydatnym narzędziem jest funkcja `debug_backtrace()`, która wyświetla ślad wywołania funkcji i plików, co jest szczególnie pomocne w przypadku błędów w skomplikowanym kodzie.

# Zobacz także

- [Praktyczne wykorzystanie funkcji debugujących w PHP](https://www.php.net/manual/en/book.debugger.php)
- [10 przydatnych funkcji debugujących w PHP](https://www.sitepoint.com/10-useful-php-debugging-functions/)
- [Podstawowe techniki debugowania w PHP](https://www.tutorialspoint.com/php/php_debugging.htm)