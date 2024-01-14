---
title:                "PHP: Generowanie wyjścia debugowania"
simple_title:         "Generowanie wyjścia debugowania"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami, podczas pisania kodu w PHP, może się zdarzyć, że napotkasz błąd lub problem, który nie jest łatwy do zlokalizowania. Wtedy przydatne może być używanie debugowania, czyli drukowanie informacji o działaniu programu, aby móc zdiagnozować problem.

## Jak to zrobić

Istnieją różne sposoby na drukowanie debug outputu w PHP, w tym:
- Słowo kluczowe `echo` - pozwala wyświetlić wartość zmiennej lub tekstu na stronie internetowej.
- Funkcja `print_r()` - przydatna w przypadku drukowania wielowymiarowych tablic lub obiektów.
- Funkcja `var_dump()` - wyświetla szczegółowe informacje o zmiennych, w tym typ, rozmiar i wartość.

Przykładowy kod wykorzystujący te funkcje wyglądałby następująco:

```PHP
$variable = "Hello world!";
echo $variable;
// Wyświetli: Hello world!

$array = array("apple", "banana", "orange");
print_r($array);
/*
Wyświetli:
Array (
    [0] => apple
    [1] => banana
    [2] => orange
)
*/

$number = 123;
var_dump($number);
// Wyświetli: int(123)
```

## Deep Dive

Wykorzystanie debugowania jest szczególnie przydatne w przypadku większych projektów, gdzie napotkane błędy mogą być trudne do zlokalizowania. Dzięki drukowaniu debug outputu możesz dokładnie prześledzić poziom wykonania kodu i znaleźć miejsce, w którym problem się pojawia.

Pamiętaj jednak, żeby nie pozostawiać wydruków debug outputu w kodzie produkcyjnym, ponieważ mogą one ujawniać poufne informacje i spowolnić działanie strony.

## Zobacz także

- [PHP Debugging Techniques](https://www.php.net/manual/en/debugger.php)
- [Debugging PHP with Xdebug](https://medium.com/the-andela-way/debugging-php-applications-the-smarter-way-using-xdebug-2a84f139c447)
- [Using Debugging Tools in PHPStorm](https://www.jetbrains.com/help/phpstorm/debugging-with-phpstorm.html)