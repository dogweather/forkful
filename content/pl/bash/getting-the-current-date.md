---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

---
title: Jak Uzyskać Aktualną Datę w Bashu?
---

## Co i Dlaczego?

Pobieranie aktualnej daty to proces odczytywania bieżących informacji o dacie i czasie. Programiści robią to, aby monitorować aktualne zdarzenia lub logować informacje.

## Jak to zrobić:

W Bashu, możemy użyć polecenia `date` do uzyskania aktualnej daty i czasu.

```Bash
# Pobranie aktualnej daty
date
```

Domyślnie, polecenie `date` zwróci pełną datę, razem z dniem tygodnia, miesiącem, dniem miesiąca, aktualną godziną, strefą czasową i rokiem.

Przykładowe wyjście:

```Bash
Thu Sep 23 15:48:00 CEST 2021
```

## Głębsze Zrozumienie

### Kontekst Historyczny
Polecenie `date` jest niesłychanie stare, istnieje w Unixie od lat 70-tych, a w Bashu od kiedy tylko Bash zaczął istnieć.

### Alternatywy
Innym sposobem na pobranie daty jest użycie polecenia `printf` wraz z funkcją `strftime` z `awk`:

```Bash
# Pobranie aktualnej daty za pomocą awk
awk "BEGIN {printf \"%s\", strftime()}"
```

### Implementacja
Polecenie `date` w Bashu jest zaimplementowane za pomocą funkcji systemowej `gettimeofday()` lub `clock_gettime()`. Używa ona strefy czasowej systemu do wygenerowania bieżącej daty i czasu.

## Zobacz Również

- [Bash Manual](https://www.gnu.org/software/bash/manual/bash.html): Dokumentacja Bash zawiera więcej szczegółów na temat polecenia `date`.
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/): Szczegółowy przewodnik po skrypcie Bash, który mówi więcej o manipulacji datą i czasem.
- [Unix Time-Stamp](https://www.unixtimestamp.com/): Narzędzie online do konwersji czasu Unix na zrozumiały format daty.