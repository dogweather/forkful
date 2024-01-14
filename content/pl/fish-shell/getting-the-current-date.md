---
title:                "Fish Shell: Uzyskiwanie bieżącej daty"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu potrzebujemy aktualnej daty. Może to być przydatne na przykład do wyświetlania daty ostatniej aktualizacji strony lub do generowania plików z nazwami zawierającymi datę. Dzięki językowi Fish Shell można to łatwo osiągnąć.

## Jak zrobić

Podstawowym poleceniem w Fish Shell do uzyskania aktualnej daty jest `date`. Polecenie to może być użyte w różnych formatach, na przykład:

```Fish Shell
date +"%d/%m/%Y"
```
W wyniku powyższego polecenia otrzymamy datę w formacie DD/MM/RRRR, na przykład: 03/05/2021.

Możemy także wyświetlić tylko rok lub miesiąc, wykorzystując odpowiednie formatowanie, na przykład:

```Fish Shell
date +"%Y" # wyświetli aktualny rok
date +"%b" # wyświetli skrócony nazwę aktualnego miesiąca (np. Jan, Feb, Mar)
```
Dzięki temu poleceniu możemy dostosować format daty do swoich potrzeb.

## Głębszy zanurzenie

Funkcja `date` w Fish Shell jest oparta na standardowym poleceniu `date` w systemie Unix. Oznacza to, że możemy wykorzystać wszystkie opcje dostępne w normalnej komendzie `date`. Na przykład możemy ustawić datę na inną niż aktualna, używając opcji `-s`, na przykład:

```Fish Shell
date -s "15 APR 2021" # ustawi datę na 15 kwietnia 2021
```
Możemy także wyłączyć automatyczne przesunięcie letniego czasu użytkując opcji `-u`, na przykład:

```Fish Shell
date -u +"%H:%M:%S" # wyświetli aktualny czas bez przesunięcia letniego
```

W ten sposób możemy bardziej zaawansowanie manipulować datą w systemie, wykorzystując możliwości standardowego polecenia `date`.

## Zobacz także

- Dokumentacja Fish Shell: https://fishshell.com/docs/current/
- Poradnik dotyczący wykorzystania polecenia `date`: https://www.computerhope.com/unix/date.htm
- Wszystkie dostępne formaty daty w poleceniu `date`: https://man7.org/linux/man-pages/man1/date.1.html