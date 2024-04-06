---
date: 2024-01-20 17:35:54.182376-07:00
description: "How to: - Jak to zrobi\u0107: ."
lastmod: '2024-04-05T21:53:37.023480-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Konwersja daty na \u0142a\u0144cuch znak\xF3w"
weight: 28
---

## How to: - Jak to zrobić:
```Bash
# Aktualna data w standardowym formacie
date_str=$(date)
echo $date_str
# Output: Wed Mar 3 14:22:10 PST 2021

# Własny format daty, np. YYYY-MM-DD
custom_date_str=$(date +"%Y-%m-%d")
echo $custom_date_str
# Output: 2021-03-03

# Drukowanie poszczególnych składników daty
echo $(date +"Today is %A, the %d of %B, %Y")
# Output: Today is Wednesday, the 03 of March, 2021
```

## Deep Dive - Wgłębienie się
Historia poleceń daty sięga Unixowych początków, będąc częścią AT&T UNIX w latach 70'. Na przestrzeni lat, 'date' zyskało więcej opcji formatowania, dzięki `strftime()` wbudowanemu w glibc. Alternatywy `date` obejmują `strftime` w innych językach, np. C, Perl, Python.

`date` korzysta z formatowania opartego na sekwencjach `%`:
- `%Y` to rok czterocyfrowy,
- `%m` to miesiąc,
- `%d` to dzień miesiąca,
- `%A` pełna nazwa dnia tygodnia,
- `%B` pełna nazwa miesiąca.

Ważne, `date` używa bieżącej strefy czasowej systemu, jeśli nie inaczej wspomniano.

## See Also - Zobacz również:
- [GNU Coreutils - Date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html) – Dokumentacja polecenia date z GNU Coreutils.
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html) – Oficjalny podręcznik do Bash.
- [Formatowanie dat w skryptach Bash](https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/) – Przykłady formatowania daty w Bashu.
