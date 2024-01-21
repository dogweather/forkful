---
title:                "Tworzenie pliku tymczasowego"
date:                  2024-01-20T17:41:08.926298-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Tworzenie tymczasowych plików to kwestia zapisywania danych, które nie muszą pozostać w systemie na stałe. Programiści wykorzystują je, aby przechowywać informacje między cyklami wykonywania programu, testować aplikacje albo jako bufor do danych generowanych dynamicznie.

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