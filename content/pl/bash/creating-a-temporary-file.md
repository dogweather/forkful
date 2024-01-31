---
title:                "Tworzenie pliku tymczasowego"
date:                  2024-01-20T17:39:41.259093-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tworzenie pliku tymczasowego"

category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Tworzenie tymczasowego pliku to sposób na zapis tymczasowych danych potrzebnych podczas działania skryptu czy programu. Programiści robią to, by nie zaśmiecać systemu stałymi plikami, które są potrzebne tylko chwilowo.

## Jak to zrobić:
W Bashu tworzenie tymczasowych plików jest banalnie proste. Użyj `mktemp` aby zadbać o unikalność i bezpieczeństwo.

```Bash
# Stwórz tymczasowy plik
tempfile=$(mktemp)

# Użyj tymczasowego pliku
echo "To jest test" > "$tempfile"
cat "$tempfile"

# Posprzątaj po skończeniu pracy
rm "$tempfile"
```

Wynik działania:
```
To jest test
```

## Więcej szczegółów:
Polecenie `mktemp` jest dostępne na systemach Unix od lat i nadal jest najlepszym wyborem dla Bash. Alternatywą mógłby być ręczne tworzenie plików z użyciem `$$` (PID procesu) w nazwie, ale to mniej bezpieczne. `mktemp` może stworzyć również tymczasowy katalog za pomocą opcji `-d`. Bardzo ważne: zawsze pamiętaj by usuwać tymczasowe pliki, aby nie pozostawiać bałaganu w systemie.

## Zobacz także:
- [Bash man page dla mktemp](https://man7.org/linux/man-pages/man1/mktemp.1.html)
- [GNU Coreutils: mktemp](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html)
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/)
