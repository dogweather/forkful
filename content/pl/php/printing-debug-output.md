---
title:                "Drukowanie komunikatów debugowania"
html_title:           "Haskell: Drukowanie komunikatów debugowania"
simple_title:         "Drukowanie komunikatów debugowania"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Drukowanie informacji do debugowania to proces, który pozwala programistom śledzić zmienne i eventy w ich kodzie. To pomaga nam zrozumieć logikę i zidentyfikować ewentualne błędy.

## Jak to zrobić:

W PHP, użyjemy funkcji `var_dump($variable);` aby wyświetlić wszelkie szczegóły o zmiennej:

```PHP
$x = "To jest przykład";
var_dump($x);
```

To zwróci:

```PHP
string(17) "To jest przykład"
```

## Podróż w głąb tematu

Drukowanie informacji do debugowania jest starsze niż większość języków programowania. Pierwsze komputery używały lampek do wyświetlania wartości.

Co do alternatyw, weźmy na przykład funkcję `print_r()`. Jest bardziej przyjazna dla człowieka niż `var_dump()`, ale nie pokazuje szczegółowych informacji o typach danych.

```PHP
print_r($x);
```

Zwraca:

```PHP
To jest przykład
```

W implementacji, `var_dump()` rzeczywiście korzysta z `zend_print_zval_r` w jądrze PHP. Ta funkcja jest obszernie opisana w dokumentacji PHP.

## Zobacz również

1. [Xdebug](https://xdebug.org/): Potężne narzędzie do debugowania PHP. Pozwala na śledzenie wykonania kodu i przechwytywanie stack trace.