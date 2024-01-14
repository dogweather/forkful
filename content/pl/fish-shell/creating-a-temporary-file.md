---
title:                "Fish Shell: Tworzenie pliku tymczasowego"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych jest często nieuniknionym etapem w pisaniu skryptów w języku Fish Shell. Ma to wiele zastosowań, na przykład przechowywanie danych tymczasowych lub tymczasowe zapisywanie wyników obliczeń.

## Jak to zrobić

Aby utworzyć tymczasowy plik w języku Fish Shell, możemy wykorzystać polecenie `mktemp`. Przykładowy kod wyglądałby następująco:

```Fish Shell
# Tworzy plik tymczasowy o nazwie example.txt
mktemp example.txt
```

Możemy także użyć opcji `-t`, aby ustawić prefiks nazwy pliku tymczasowego, na przykład:

```Fish Shell
# Tworzy plik tymczasowy o nazwie temp_XXX.txt
mktemp -t temp_XXX.txt
```

Możemy również określić ścieżkę do której ma zostać zapisany plik tymczasowy za pomocą opcji `-p`, na przykład:

```Fish Shell
# Tworzy plik tymczasowy o nazwie example.txt w katalogu /tmp
mktemp -p /tmp example.txt
```

## Deep Dive

Podczas gdy polecenie `mktemp` pozwala nam na szybkie utworzenie pliku tymczasowego, istnieje wiele innych sposobów na obsługę manipulacji i zarządzania tymczasowymi plikami w języku Fish Shell. Na przykład, możemy wykorzystać zmienne `TMPDIR` lub `PWD` do ustalenia katalogu, w którym ma zostać utworzony plik tymczasowy lub nadanie mu wyjątkowych uprawnień za pomocą polecenia `chmod`.

## Zobacz również

- Dokumentacja języka Fish Shell: https://fishshell.com/docs/current/index.html
- Przewodnik po pisaniu skryptów w języku Fish Shell: https://fishshell.com/docs/current/tutorial.html
- Lista przydatnych poleceń w języku Fish Shell: https://fishshell.com/docs/current/commands.html