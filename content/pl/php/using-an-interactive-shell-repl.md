---
title:                "Korzystanie z interaktywnego shella (REPL)"
aliases:
- pl/php/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:16:46.640989-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z interaktywnego shella (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Interaktywna powłoka, czyli REPL (Read-Eval-Print Loop - Pętla Czytaj-Ewaluuj-Wypisz) pozwala na pisania i uruchamianie kodu PHP na bieżąco. Jest to idealne do eksperymentów, debugowania czy nauki, ponieważ można testować fragmenty kodu bez konieczności tworzenia pełnego skryptu.

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
