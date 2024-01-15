---
title:                "Wydrukowanie danych do debugowania"
html_title:           "PHP: Wydrukowanie danych do debugowania"
simple_title:         "Wydrukowanie danych do debugowania"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Debugowanie jest nieodłączną częścią tworzenia aplikacji, a wiedza na temat drukowania informacji debugujących jest niezbędna dla każdego programisty. Dzięki temu narzędziu można szybciej znaleźć i naprawić błędy w kodzie, co przyczynia się do poprawy jakości tworzonego oprogramowania.

## Jak to zrobić

Aby drukować informacje debugujące w języku PHP, należy skorzystać z funkcji `echo` lub `print_r`. Oba te polecenia służą do wypisywania danych na ekranie, jednak `print_r` jest przydatniejsze w przypadku drukowania złożonych struktur danych, takich jak tablice czy obiekty.

Przykładowy kod wykorzystujący funkcję `echo`:

```PHP
$zmienna = "Hello World!";
echo $zmienna; // wyświetli "Hello World!" na ekranie
```

A teraz przykład wykorzystujący funkcję `print_r`:

```PHP
$tablica = array("jabłko", "banan", "pomarańcza");
print_r($tablica); // wyświetli zawartość tablicy w formie czytelnej dla człowieka
```

Można także skorzystać z funkcji `var_dump`, która oprócz wyświetlenia wartości danej zmiennej, pokazuje również jej typ oraz długość (w przypadku tablic).

```PHP
$zmienna = "Hello World!";
var_dump($zmienna); // wyświetli "string(12) "Hello World!""
```

Kolejną przydatną funkcją jest `error_log`, która pozwala na zapisanie informacji debugujących do pliku zamiast wyświetlania ich na ekranie. Użyteczne, gdy nie chcemy pokazywać użytkownikom błędów lub gdy nie mamy dostępu do konsoli.

## Pogłębione informacje

Drukowanie informacji debugujących może być także przydatne przy tworzeniu aplikacji w trybie produkcyjnym. W takiej sytuacji warto skorzystać z funkcji `ini_set`, która pozwala na wyświetlenie błędów na ekranie lub zapisanie ich do pliku, bez potrzeby modyfikowania ustawień serwera.

```PHP
ini_set('display_errors', 1); // wyświetli błędy na ekranie
ini_set('log_errors', 1); // zapisze błędy do pliku
```

Pamiętaj jednak, aby wyłączyć tę funkcjonalność wersji produkcyjnej aplikacji, aby uniknąć wyświetlania wrażliwych informacji użytkownikom.

## Zobacz także

- [Dokumentacja PHP: Debugging Functions](https://www.php.net/manual/en/ref.errorfunc.php)
- [Blog SitePoint: PHP Debugging with echo and print_r](https://www.sitepoint.com/php-debugging-echo-print-r/)
- [Dokumentacja PHP: ini_set](https://www.php.net/manual/en/function.ini-set.php)