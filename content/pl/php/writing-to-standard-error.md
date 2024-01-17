---
title:                "Pisanie do standardowego błędu"
html_title:           "PHP: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O czym i dlaczego?
Pisanie do standardowego błędu jest jedną z podstawowych technik programowania w PHP. Polega ono na przekazywaniu informacji o błędach i ostrzeżeniach do odpowiedniego miejsca, zamiast wyświetlać je bezpośrednio na stronie internetowej. Jest to ważna praktyka w budowaniu bezpiecznego i niezawodnego oprogramowania.

## Jak to zrobić:
Przykład kodu:
```PHP
<?php
ini_set('display_errors', 0);
ini_set('log_errors', 1);
ini_set('error_log', '/var/www/example.com/php-error-log.txt');
```
Przykładowy tekst błędu w pliku "php-error-log.txt":
```bash
[Fri Sep 17 08:44:58 2021] [error] [client 127.0.0.1] PHP Warning: Undefined index: email in /var/www/example.com/login.php on line 10
```

## Głębszy wgląd:
Ta technika została wprowadzona w PHP już w wersji 4.3 i jest często stosowana w profesjonalnych projektach. Alternatywą dla pisania do standardowego błędu jest wyświetlanie błędów bezpośrednio na stronie internetowej, jednak jest to niebezpieczne z uwagi na potencjalne ujawnienie informacji o działającej aplikacji. Implementacja polega na używaniu różnych funkcji PHP, takich jak `ini_set` i `error_log`, aby ustawić odpowiednie ustawienia dotyczące logowania błędów.

## Zobacz także:
- [Dokumentacja PHP dla funkcji `ini_set`](https://www.php.net/manual/en/function.ini-set.php)
- [Artykuł na temat pisania do standardowego błędu w PHP](https://www.php.net/manual/en/features.errors.php)
- [Inne sposoby obsługi błędów w PHP](https://www.php.net/manual/en/ref.errorfunc.php)