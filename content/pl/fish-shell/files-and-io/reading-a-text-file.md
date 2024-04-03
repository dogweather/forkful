---
date: 2024-01-20 17:54:16.978763-07:00
description: "How to: W Fish, u\u017Cyj `read` dla interakcji i `cat` do prostej lektury."
lastmod: '2024-03-13T22:44:35.859182-06:00'
model: gpt-4-1106-preview
summary: "W Fish, u\u017Cyj `read` dla interakcji i `cat` do prostej lektury."
title: Odczytywanie pliku tekstowego
weight: 22
---

## How to:
W Fish, użyj `read` dla interakcji i `cat` do prostej lektury:

```Fish Shell
# Odczyt zawartości pliku
cat moj_plik.txt

# Przykładowe dane z pliku "witaj.txt" zawierającego "Witaj, świecie!"
cat witaj.txt
# Output: Witaj, świecie!
```

## Deep Dive
W przeszłości używaliśmy różnych komend Unix, jak `cat`, `less`, `more`, `tail`, `head`, aby czytać pliki tekstowe. Fish, chociaż oferuje własne, bardziej zaawansowane narzędzia, często opiera się na tych standardowych komendach. Alternatywą jest użycie pętli `while`, która czyta plik linia po linii:

```Fish Shell
# Czytanie pliku linia po linii
while read -la line
    echo $line
end < moj_plik.txt
```

Ważnym aspektem jest kodowanie (np. UTF-8), odpowiada ono za prawidłowe wyświetlanie znaków.

## See Also
- [Fish Documentation](https://fishshell.com/docs/current/index.html)
