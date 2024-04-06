---
date: 2024-01-20 17:41:08.926298-07:00
description: "Jak to zrobi\u0107: Wynik dzia\u0142ania programu."
lastmod: '2024-04-05T21:53:36.417653-06:00'
model: gpt-4-1106-preview
summary: "Wynik dzia\u0142ania programu."
title: Tworzenie pliku tymczasowego
weight: 21
---

## Jak to zrobić:
```Python
import tempfile

# Stwórz tymczasowy plik
with tempfile.TemporaryFile(mode='w+t') as tmp:
    # Zapisz dane do pliku
    tmp.write('Witaj, tymczasowy świecie!\n')
    # Wróć na początek pliku przed odczytem
    tmp.seek(0)
    # Wczytaj i wydrukuj zawartość
    print(tmp.read())

# Plik został już zamknięty i automatycznie usunięty
```

Wynik działania programu:
```
Witaj, tymczasowy świecie!
```

## Szczegółowe spojrzenie:
Tymczasowe pliki były używane od początków programowania. W dawnych czasach, gdy pamięć była na wagę złota, umożliwiały przechowywanie danych przekraczających jej rozmiar. Alternatywą dla tworzenia tymczasowych plików jest wykorzystywanie pamięci podręcznej, co działa szybciej, ale jest ograniczone wielkością pamięci RAM. Szczegółowość implementacji w Pythonie opiera się o moduł `tempfile`, który zapewnia bezpieczeństwo (unikanie konfliktów nazw plików) i automatyzację (usuwanie plików po zakończeniu pracy).

## Zobacz też:
- Dokumentacja modułu `tempfile`: https://docs.python.org/3/library/tempfile.html
- Stack Overflow — dyskusje o tymczasowych plikach i najlepsze praktyki: https://stackoverflow.com/questions/tagged/temporary-files
