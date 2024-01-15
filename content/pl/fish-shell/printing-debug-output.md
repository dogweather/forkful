---
title:                "Drukowanie wyjścia debugowania"
html_title:           "Fish Shell: Drukowanie wyjścia debugowania"
simple_title:         "Drukowanie wyjścia debugowania"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami podczas pisania kodu lub wykonując pracę na komputerze napotykamy problemy lub błędy, które trudno jest nam zlokalizować. W takiej sytuacji przydatnym narzędziem jest wypisywanie informacji debuggowych, aby lepiej zrozumieć co się dzieje w naszym programie. W tym artykule pokażemy jak wykorzystać wbudowane funkcje w Fish Shell do drukowania debug outputu.

## Jak to zrobić

W Fish Shell istnieją dwa sposoby na drukowanie informacji debuggowych:

1. Użyj komendy `echo` aby wyświetlić zadany tekst w terminalu:

```Fish Shell
echo "Hello world"
```

2. Użyj funkcji `debug` aby przekierować wyniki wywołania polecenia do strumienia błędów. W ten sposób możesz wyświetlić wyniki bez przerywania normalnego przebiegu programu:

```Fish Shell
debug ls -l
```

Możesz również łączyć te dwa sposoby aby wyświetlać dokładniejsze informacje debugowe.

## Deep Dive

W funkcji `debug` można również ustawić poziom logowania za pomocą flagi `-l` oraz wybrać na jakim strumieniu błędów mają być wyświetlane wyniki. Domyślnie poziom logowania jest ustawiony na `info`, ale możesz zmienić go na `error`, `warning`, `debug` lub `trace`. Możesz również przekierować wyniki na dowolny strumień błędów, np. `stderr` lub `stdout`. 

Możliwość ustawienia poziomów logowania i celu wyświetlania wyników czyni funkcję `debug` bardzo użytecznym narzędziem do debuggowania programów.

## Zobacz także

- Dokumentacja Fish Shell: https://fishshell.com/docs/current/
- Przewodnik po Fish Shell: https://fishshell.com/docs/current/tutorial.html
- Forum Fish Shell: https://github.com/fish-shell/fish-shell/issues