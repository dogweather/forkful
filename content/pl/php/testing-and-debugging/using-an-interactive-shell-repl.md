---
date: 2024-01-26 04:16:46.640989-07:00
description: "Jak to zrobi\u0107: Uruchom PHP REPL wpisuj\u0105c `php -a` w terminalu.\
  \ Oto przyk\u0142adowe dzia\u0142anie."
lastmod: '2024-03-13T22:44:35.498778-06:00'
model: gpt-4-0125-preview
summary: "Uruchom PHP REPL wpisuj\u0105c `php -a` w terminalu."
title: Korzystanie z interaktywnego shella (REPL)
weight: 34
---

## Jak to zrobić:
Uruchom PHP REPL wpisując `php -a` w terminalu. Oto przykładowe działanie:

```php
php > echo "Hello, World!";
Hello, World!
php > $arr = [1, 2, 3];
php > print_r($arr);
Array
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

Możesz również definiować funkcje:

```php
php > function sum($a, $b) { return $a + $b; }
php > echo sum(5, 10);
15
```

## W głębi
REPL-e istnieją w jakiejś formie od wczesnych lat 60. XX wieku, począwszy od LISP-a. Interaktywna powłoka PHP jest mniej zaawansowana w porównaniu do tych z języków takich jak Python czy JavaScript. Nie zachowuje stanu pomiędzy sesjami i brakuje funkcji takich jak auto-układanie kodu. Dla bardziej rozbudowanego REPL-a PHP, rozważ alternatywy jak `psysh` czy `boris`. Te zewnętrzne powłoki oferują lepsze narzędzia introspekcyjne, auto-układanie kodu, a nawet debugger.

Pod spodem, REPL PHP działa poprzez kompilowanie i wykonanie każdej linii kodu, jak jest wprowadzana. Ograniczenia tego podejścia stają się oczywiste przy rzeczach takich jak redeklarowanie klas, co nie jest możliwe w tej samej sesji. To świetnie sprawdza się do prostych testów, ale może być uciążliwe przy bardziej skomplikowanych zadaniach.

## Zobacz również
- [Podręcznik PHP - Interaktywna powłoka](https://www.php.net/manual/en/features.commandline.interactive.php)
- [PsySH: Konsola programisty w czasie wykonania, interaktywny debugger i REPL dla PHP](https://psysh.org/)
- [Boris: Miniaturowy REPL dla PHP](https://github.com/borisrepl/boris)
