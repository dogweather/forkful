---
title:                "Pobieranie aktualnej daty"
html_title:           "Bash: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Aktualna data jest informacją o bieżącym dniu, miesiącu, roku i czasie, która jest wykorzystywana przez programistów do wielu celów, takich jak datowanie plików, rejestracja zdarzeń, synchronizacja czasu i wiele innych.

## Jak to zrobić?

Aby uzyskać bieżącą datę w skrypcie Bash, wystarczy użyć polecenia `date`. Przykładowy kod wyglądałby następująco:

```Bash
current_date=$(date +'%Y-%m-%d')
echo "Dzisiaj jest $current_date"
```

W przykładzie powyżej, używając formatu `%Y-%m-%d`, otrzymamy odpowiednią datę w formacie rok-miesiąc-dzień. Istnieje wiele innych formatów, które można wykorzystać, w zależności od potrzeb.

## Odkrywanie głębszych tajemnic

Polecenie `date` jest dostępne w większości systemów operacyjnych i zostało stworzone przez Kena Thompsona w 1971 roku. Istnieją również inne sposoby na uzyskanie aktualnej daty, takie jak użycie wbudowanych funkcji języka, na przykład w Pythonie czy Javie.

## Zobacz także

- [Dokumentacja polecenia `date` w manualu Bash](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Inne sposoby na uzyskanie aktualnej daty w skrypcie Bash](https://www.linuxjournal.com/content/doing-date-math-command-line)
- [Alternatywne sposoby przechowywania i reprezentowania daty w bazach danych](https://www.oracle.com/technetwork/articles/sql/18-nodates-082075.html)