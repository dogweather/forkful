---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Odczytywanie argumentów wiersza poleceń to proces, w którym skrypt PHP interpretuje dane wejściowe z konsoli. Programiści wykonują ten proces, aby móc sterować zachowaniem skryptu przy różnych parametrach wejściowych.

## Jak to zrobić:

Odczyt argumentów z wiersza poleceń w PHP jest bardzo prosty. PHP dostarcza specjalną zmienną globalną ```$argv```, która jest tablicą i zawiera wszelkie argumenty przekazane do skryptu.

```PHP
<?php
print_r($argv);
?>
```

Możesz uruchomić powyższy skrypt z dowolną ilością argumentów:

```shell
$ php script.php arg1 arg2 arg3
```

Wynik powinien wyglądać tak:

```shell
Array
(
    [0] => script.php
    [1] => arg1
    [2] => arg2
    [3] => arg3
)
```

## Głębsze spojrzenie 

Mimo, że ```$argv``` jest dość prosty w użyciu, ważne jest zrozumienie, że jest to dość stary sposób interakcji z argumentami wiersza poleceń. Został on wprowadzony do PHP 4.0.0 w 2000 roku. Istnieją inne, bardziej zaawansowane metody manipulowania argumentami wiersza poleceń, takie jak biblioteka `getopt` czy symfony/console, które oferują bardziej elastyczne i wydajne narzędzia.

Kiedy używasz ```$argv```, warto pamiętać, że ```$argv[0]``` zawsze będzie nazwą skryptu, który jest uruchamiany. Rzeczywiste argumenty zaczynają się od ```$argv[1]```.

## Zobacz też 

- [PHP.net: Predefined Variables - $argc & $argv](https://www.php.net/manual/en/reserved.variables.argv.php)
- [The PHP `getopt` function](https://www.php.net/manual/en/function.getopt.php)
- [Symfony's Console Component](https://symfony.com/doc/current/components/console.html)