---
title:                "Pisanie do standardowego błędu"
html_title:           "Bash: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i dlaczego
Pisanie do standardowego wyjścia błędów jest częstym zabiegiem wśród programistów Bash. Polega on na przesyłaniu informacji o błędach do specjalnego strumienia, który jest wyświetlany w konsoli lub zapisywany do pliku. Jest to przydatne do monitorowania działania skryptów i łatwiejszego debugowania.

## Jak to zrobić
Możesz użyć polecenia `>&2` lub `2>&1` aby przekierować wyjście błędów do standardowego wyjścia. Na przykład:

```Bash
ls nieistniejacy_plik 2>&1
```
Powyższa komenda spowoduje wyświetlenie błędu w konsoli zamiast standardowego wyjścia błędów.

## Głębszy zanurzenie
Pisanie do standardowego wyjścia błędów jest powszechną praktyką w programowaniu od lat 70. Alternatywą dla tego podejścia jest używanie funkcji `echo` lub `printf` do wyświetlania błędów, jednak pisanie do standardowego wyjścia błędów jest bardziej wydajne i zapewnia lepszą kontrolę nad wyświetlanymi informacjami.

## Zobacz też
Linki do podobnych źródeł:
- [Dokumentacja Bash](https://www.gnu.org/software/bash/manual/bash.html#Redirections)