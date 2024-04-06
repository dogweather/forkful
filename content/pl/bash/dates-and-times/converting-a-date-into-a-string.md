---
date: 2024-01-20 17:35:54.182376-07:00
description: "How to: - Jak to zrobi\u0107: Historia polece\u0144 daty si\u0119ga\
  \ Unixowych pocz\u0105tk\xF3w, b\u0119d\u0105c cz\u0119\u015Bci\u0105 AT&T UNIX\
  \ w latach 70'. Na przestrzeni lat, 'date' zyska\u0142o wi\u0119cej\u2026"
lastmod: '2024-04-05T22:50:49.922218-06:00'
model: gpt-4-1106-preview
summary: "- Jak to zrobi\u0107: Historia polece\u0144 daty si\u0119ga Unixowych pocz\u0105\
  tk\xF3w, b\u0119d\u0105c cz\u0119\u015Bci\u0105 AT&T UNIX w latach 70'. Na przestrzeni\
  \ lat, 'date' zyska\u0142o wi\u0119cej opcji formatowania, dzi\u0119ki `strftime()`\
  \ wbudowanemu w glibc. Alternatywy `date` obejmuj\u0105 `strftime` w innych j\u0119\
  zykach, np. C, Perl, Python. `date` korzysta z formatowania opartego na sekwencjach\
  \ `%`: - `%Y` to rok czterocyfrowy, - `%m` to miesi\u0105c, - `%d` to dzie\u0144\
  \ miesi\u0105ca, - `%A` pe\u0142na nazwa dnia tygodnia, - `%B` pe\u0142na nazwa\
  \ miesi\u0105ca. Wa\u017Cne, `date` u\u017Cywa bie\u017C\u0105cej strefy czasowej\
  \ systemu, je\u015Bli nie inaczej wspomniano."
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
