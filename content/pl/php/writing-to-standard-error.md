---
title:                "PHP: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas pisania kodu w języku PHP możemy natrafić na błędy, które są wyświetlane w standardowym wyjściu. Jednakże, w niektórych przypadkach, zamiast wyświetlać błąd dla użytkownika, lepiej jest zapisać go do standardowego błędu. W tym artykule dowiesz się dlaczego warto pisać do standardowego błędu i jak to zrobić.

## Jak to zrobić

Aby zapisać błąd do standardowego wyjścia, możemy użyć funkcji `fwrite(STDERR, "Tekst błędu");`. Funkcja ta przyjmuje dwa argumenty - uchwyt do standardowego wyjścia błędu ( `STDERR`) oraz tekst, który chcemy zapisać. Spróbujmy teraz wywołać tę funkcję w kodzie PHP.

```PHP
<?php
fwrite(STDERR, "To jest przykładowy błąd.");
echo "Ten tekst pojawi się w standardowym wyjściu.";
```

Teraz, gdy wykonamy ten kod, zamiast wyświetlić tekst "Ten tekst pojawi się w standardowym wyjściu.", zostanie on zapisany w standardowym wyjściu błędu, a użytkownik zobaczy tylko błąd "To jest przykładowy błąd.".

## Dogłębna analiza

Pisanie do standardowego błędu jest szczególnie przydatne w sytuacjach, gdy nie chcemy wyświetlać użytkownikowi błędów lub gdy chcemy zachować jasność w standardowym wyjściu. Możemy również wykorzystać funkcję `error_log()` do zapisywania błędów do pliku dziennika. Jest to przydatne w przypadku, gdy chcemy później przeanalizować i zrozumieć błędy w naszym kodzie.

## Zobacz też

- [Dokumentacja PHP - fwrite()](https://www.php.net/manual/en/function.fwrite.php)
- [Dokumentacja PHP - error_log()](https://www.php.net/manual/en/function.error-log.php)