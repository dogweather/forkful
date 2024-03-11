---
date: 2024-01-20 17:54:16.978763-07:00
description: "Czytanie pliku tekstowego to proces wydobywania danych z pliku zapisanego\
  \ w formacie tekstowym. Programi\u015Bci to robi\u0105, aby obs\u0142ugiwa\u0107\
  \ konfiguracje,\u2026"
lastmod: '2024-03-11T00:14:09.072812-06:00'
model: gpt-4-1106-preview
summary: "Czytanie pliku tekstowego to proces wydobywania danych z pliku zapisanego\
  \ w formacie tekstowym. Programi\u015Bci to robi\u0105, aby obs\u0142ugiwa\u0107\
  \ konfiguracje,\u2026"
title: Odczytywanie pliku tekstowego
---

{{< edit_this_page >}}

## What & Why?
Czytanie pliku tekstowego to proces wydobywania danych z pliku zapisanego w formacie tekstowym. Programiści to robią, aby obsługiwać konfiguracje, wczytywać dane wejściowe, przetwarzać logi czy też po prostu odczytywać treść.

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
