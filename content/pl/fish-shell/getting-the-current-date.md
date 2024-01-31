---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:14:18.966895-07:00
html_title:           "Bash: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"

category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pobranie aktualnej daty to podstawowa operacja polegająca na uzyskaniu informacji o bieżącym dniu, miesiącu i roku. Programiści robią to z wielu powodów, od logowania i timestampów po wyzwalanie zdarzeń związanych z czasem.

## Jak to zrobić:
```Fish Shell
# Pobranie bieżącej daty w standardowym formacie
set current_date (date)
echo $current_date
# Przykładowe wyjście: Śro, 20 Kwi 2023 14:35:58

# Pobranie składników daty po kolei
set day (date "+%d")
set month (date "+%m")
set year (date "+%Y")
echo "$day/$month/$year"
# Przykładowe wyjście: 20/04/2023
```

## Uważne spojrzenie
Pobieranie daty to stary jak świat koncept w programowaniu. W systemach Unix do tej pory używa się komendy `date`. Alternatywą może być językowy sposób dostępu do daty poprzez wbudowane biblioteki, jak np. `datetime` w Pythonie. Implementacja i formatowanie daty są zależne od systemu oraz ustawień regionalnych – warto o tym pamiętać, zwłaszcza przy pisaniu skryptów międzyplatformowych.

## Zobacz też
- Pełna dokumentacja Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Informacje o poleceniu `date` na stronie man: [https://man7.org/linux/man-pages/man1/date.1.html](https://man7.org/linux/man-pages/man1/date.1.html)
- Tutorial dotyczący skryptów Fish: [https://fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)
