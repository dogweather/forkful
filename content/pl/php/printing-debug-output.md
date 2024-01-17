---
title:                "Dr"
html_title:           "PHP: Dr"
simple_title:         "Dr"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## O co chodzi i po co?

W programowaniu, drukowanie wyników debugowania jest ważną częścią procesu tworzenia oprogramowania. Polega to na wyświetlaniu informacji w celu zidentyfikowania błędów i poprawienia działania programu. Programiści drukują wyniki debugowania, aby znaleźć miejsca, w których program nie działa zgodnie z oczekiwaniami.

## Jak to zrobić?

W PHP istnieje kilka sposobów na drukowanie informacji debugowania. Najprostszym jest użycie funkcji `echo` lub `print`. Można też użyć funkcji `var_dump` lub `print_r`, które wyświetlą strukturę danego obiektu lub zmiennej. Przykłady:

```PHP
// użycie funkcji echo
$imie = "Anna";
echo "Witaj, $imie!"; // wyświetli "Witaj, Anna!"

// użycie funkcji var_dump
$dane = array(1, 2, 3);
var_dump($dane); // wyświetli strukturę tablicy: "array(3) { [0]=> int(1) [1]=> int(2) [2]=> int(3) }"
```

## W głębi tematu

Drukowanie informacji debugowania jest powszechną praktyką w programowaniu od lat. Wcześniej programiści musieli polegać na wyświetlaniu wyników na konsoli lub korzystaniu z modułów zewnętrznych, ale wraz z rozwojem PHP, funkcje `echo`, `print`, `var_dump` i `print_r` zostały włączone do języka.

Alternatywą dla drukowania debug output jest użycie debuggera, czyli narzędzia służącego do analizy i debugowania kodu. Umożliwia ono wstrzymywanie wykonania programu w wybranym miejscu i obserwowanie wartości zmiennych oraz wykonanych kroków, co może być bardziej wygodne niż ręczne drukowanie outputu.

Implementacja funkcji drukowania jest dość prosta, ponieważ są one już zaimplementowane w języku PHP. Zwykle nie wymagają żadnych dodatkowych ustawień lub konfiguracji.

## Zobacz też

- Dokumentacja PHP dotycząca funkcji [echo](https://www.php.net/manual/en/function.echo.php), [print](https://www.php.net/manual/en/function.print.php), [var_dump](https://www.php.net/manual/en/function.var-dump.php) i [print_r](https://www.php.net/manual/en/function.print-r.php)
- [Wprowadzenie do debugowania w PHP](https://www.thesitewizard.com/php/debugging-php-script.shtml)