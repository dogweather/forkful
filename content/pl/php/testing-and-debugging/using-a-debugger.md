---
date: 2024-01-26 03:50:42.497718-07:00
description: "Jak to zrobi\u0107: PHP jest wyposa\u017Cony w interaktywny debugger\
  \ zwany Xdebug. Oto jak go u\u017Cywa\u0107. Po pierwsze, upewnij si\u0119, \u017C\
  e masz zainstalowany i\u2026"
lastmod: '2024-03-13T22:44:35.501948-06:00'
model: gpt-4-0125-preview
summary: "PHP jest wyposa\u017Cony w interaktywny debugger zwany Xdebug."
title: Korzystanie z debugera
weight: 35
---

## Jak to zrobić:
PHP jest wyposażony w interaktywny debugger zwany Xdebug. Oto jak go używać.

Po pierwsze, upewnij się, że masz zainstalowany i skonfigurowany Xdebug w swoim pliku `php.ini`:

```
zend_extension=/usr/local/lib/php/extensions/no-debug-non-zts-xxxxxxxx/xdebug.so
xdebug.mode=debug
xdebug.start_with_request=yes
```

Następnie, napisz prosty skrypt PHP z błędem:

```PHP
<?php
function add($a, $b) {
    return $a - $b; // Ups! To powinien być plus, a nie minus
}

$result = add(1, 2);
echo "Wynik to: $result"; // Wynik powinien być 3, a nie -1
```

Korzystając z IDE takiego jak PhpStorm, ustaw punkt przerwania klikając obok numeru linii. Uruchom debugger i obserwuj, jak zmieniają się zmienne, gdy przeskakujesz przez wykonanie. Gdy przejdziesz przez funkcję `add`, zauważysz, że `$result` staje się -1, co jest nieoczekiwane.

## Dogłębna analiza:
Historycznie, PHP był używany głównie do małych skryptów i debugowanie polegało na dodawaniu instrukcji `var_dump()` i `print_r()` w kodzie. Z czasem, gdy PHP stał się kluczowym graczem w rozwoju stron internetowych, zaczęto używać bardziej zaawansowanych narzędzi takich jak Xdebug i Zend Debugger.

Alternatywy dla Xdebuga to między innymi pcov i phpdbg. Oferują one różne funkcje, ale mogą nie być tak kompleksowe jak Xdebug. Phpdbg to lekki debugger specyficzny dla PHP, który jest dystrybuowany z PHP od wersji 5.6, a pcov to sterownik pokrycia kodu.

Przy wdrażaniu debugera pamiętaj, że nigdy nie powinieneś zostawiać włączonego debugera na swoim serwerze produkcyjnym, gdyż może to narazić na szwank bezpieczeństwo i spowolnić wydajność.

## Zobacz również:
- [Dokumentacja Xdebuga](https://xdebug.org/docs/)
- [Przewodnik po debugowaniu PhpStorm](https://www.jetbrains.com/help/phpstorm/debugging.html)
- [PHP.net o phpdbg](https://www.php.net/manual/en/book.phpdbg.php)
- [pcov na GitHubie](https://github.com/krakjoe/pcov)
