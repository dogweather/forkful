---
title:                "PHP: Pisanie do standardowego błędu"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego pisanie do błędu standardowego jest ważne?

Pisanie do standardowego błędu może być przydatne dla programistów, którzy chcą od razu reagować na błędy podczas wykonywania swojego kodu. Może to pomóc w szybszym znajdowaniu i naprawianiu problemów, co przyspieszy proces debugowania i poprawy kodu.

## Jak to zrobić?

```PHP
<?php
fwrite(STDERR, "Przykładowy błąd, nieprawidłowe wywołanie funkcji"); // pisanie do standardowego błędu
```

Wywołanie funkcji "fwrite()" z argumentem STDERR spowoduje, że tekst zostanie wypisany bezpośrednio do standardowego błędu. Można to wykorzystać w warunkach "if" lub "try-catch" do łatwego wyświetlania informacji o błędzie.

## Głębszy wgląd w pisanie do standardowego błędu

Pisanie do standardowego błędu można wykorzystać także do generowania własnych komunikatów błędów. Można zdefiniować swoją własną funkcję obsługującą błędy, która będzie wywoływana zamiast standardowej, wbudowanej w PHP. W ten sposób można dostosować format i treść komunikatów błędów według własnych potrzeb.

## Zobacz także

- [Oficjalna dokumentacja PHP o pisaniu do standardowego błędu](https://www.php.net/manual/en/function.fwrite.php)
- [Poradnik na temat pisania do błędu standardowego na stronie Stack Overflow](https://stackoverflow.com/questions/6631193/make-output-to-stream-error-instead-of-standard-output)
- [Wideo na YouTube o pisanie do standardowego błędu](https://www.youtube.com/watch?v=zgnQjazsaqI)