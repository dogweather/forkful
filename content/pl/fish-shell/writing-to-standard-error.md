---
title:                "Pisanie do standardowego błędu"
date:                  2024-01-19
simple_title:         "Pisanie do standardowego błędu"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Standardowy błąd (stderr) to osobny strumień, którym program komunikuje błędy i ostrzeżenia, nie mieszając ich z głównymi danymi wyjściowymi (stdout). Użycie stderr pozwala zarządzać danymi i błędami efektywnie, zwłaszcza przy przekierowywaniu wyjścia.

## Jak to zrobić:
Wypisujemy na standardowy błąd przez przekierowanie `2>`. Przykład:

```Fish Shell
echo "To jest normalne wyjście"
echo "To jest błąd" 1>&2
```

Oczekiwane wyjście:
```
To jest normalne wyjście
```

Błędy będą wypisane na ekran, ale nie widać ich tutaj, bo nie są częścią normalnego wyjścia. Sprawdź to w swoim terminalu.

## Głębiej:
Fish Shell używa składni podobnej do Bash do przekierowania stderr. Historia tej funkcji sięga początków interfejsów wiersza poleceń, gdzie rozdzielenie błędów od danych wyjściowych było kluczowe dla efektywnej pracy. Zamiast `2>`, można użyć `^`, ale to rozwiązanie jest przestarzałe w najnowszych wersjach Fish. Na systemach Unixowych, standardowo są trzy strumienie: wejście (stdin), wyjście (stdout) i błąd (stderr).

## Zobacz również:
- Dokumentacja Fish Shell o przekierowaniach: https://fishshell.com/docs/current/index.html#redirections
- Instrukcja GNU o przekierowaniach: https://www.gnu.org/software/bash/manual/html_node/Redirections.html
- Tutorial o obsłudze stderr w różnych powłokach: https://www.cyberciti.biz/faq/redirecting-stderr-to-stdout/
