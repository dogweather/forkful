---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pobieranie Aktualnej Daty w Fish Shell

## Co i Dlaczego?

Pobieranie aktualnej daty polega na wywołaniu funkcji przekazującej rzeczywistą datę i czas w wyznaczonym formacie. Programiści robią to na wiele powodów, np. do oznaczania wyników logów, śledzenia czasu wykonywania zadań, czy tworzenia plików kopii zapasowych z datą w nazwie.

## Jak To Zrobić:

Oto kilka prostych przykładów uzyskania aktualnej daty w Fish Shell:

```Fish Shell
# Pobranie aktualnej daty
echo (date)

# Pobranie aktualnej daty w formacie YYYY-MM-DD
echo (date "+%Y-%m-%d")
```

Wyjście takiego kodu może wyglądać na przykład tak:

```Fish Shell
# Wyjście dla echo (date)
Sun Sep 12 17:13:44 CEST 2021

# Wyjście dla echo (date "+%Y-%m-%d")
2021-09-12
```

## Pod Dno:

Fish Shell, czyli Friendly Interactive Shell, to interaktywna powłoka dla UNIX-a, która wyszła w 2005 roku. Zawiera wiele funkcji, w tym wbudowane obsługi daty i czasu za pomocą komendy `date`, którą widzieliśmy w naszych przykładach.

Alternatywą dla wykorzystania 'date' w Fish jest wykorzystanie innych narzędzi systemowych lub zewnętrznych. Na przykład, jeśli korzystasz także z Pythona, możesz wykorzystać moduł `datetime`.

Szczegóły implementacji komendy `date` różnią się w zależności od systemu. Na systemach Unixowych `date` jest zwykle częścią pakietu coreutils, a jego kod źródłowy jest dostępny do przejrzenia i modyfikacji.

## Zobacz Też:

- Dokumentacja Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Manual GNU coreutils `date`: [https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- Python `datetime` module: [https://docs.python.org/3/library/datetime.html](https://docs.python.org/3/library/datetime.html)