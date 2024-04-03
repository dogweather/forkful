---
date: 2024-01-20 17:39:41.259093-07:00
description: "Jak to zrobi\u0107: W Bashu tworzenie tymczasowych plik\xF3w jest banalnie\
  \ proste. U\u017Cyj `mktemp` aby zadba\u0107 o unikalno\u015B\u0107 i bezpiecze\u0144\
  stwo."
lastmod: '2024-03-13T22:44:35.604701-06:00'
model: gpt-4-1106-preview
summary: "W Bashu tworzenie tymczasowych plik\xF3w jest banalnie proste."
title: Tworzenie pliku tymczasowego
weight: 21
---

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
