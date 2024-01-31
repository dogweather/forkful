---
title:                "Zapisywanie pliku tekstowego"
date:                  2024-01-19
simple_title:         "Zapisywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Zapisywanie do pliku tekstowego to zapisywanie danych w formie tekstowej na dysku. Programiści robią to, by trwale zapisywać stan aplikacji, logi, konfiguracje czy wymieniać dane między programami.

## Jak to zrobić:
```Python
# Otwarcie pliku do zapisu (jeśli plik nie istnieje, zostanie utworzony)
with open('przykladowy_plik.txt', 'w', encoding='utf-8') as plik:
    plik.write("Cześć, to jest tekst w pliku!\n")
    plik.write("Dodajemy kolejną linię tekstu.")

# Otwierając plik w trybie 'a', dopisujemy do istniejącego pliku
with open('przykladowy_plik.txt', 'a', encoding='utf-8') as plik:
    plik.write("\nA tutaj tekst dopisany.")
 
# Weryfikacja zawartości pliku
with open('przykladowy_plik.txt', 'r', encoding='utf-8') as plik:
    zawartosc = plik.read()
    print(zawartosc)

"""
Cześć, to jest tekst w pliku!
Dodajemy kolejną linię tekstu.
A tutaj tekst dopisany.
"""
```

## W głąb tematu:
Koncepcja plików istnieje od początków informatyki. W Pythonie zapis do pliku jest prosty dzięki wbudowanym metodą `write()`. Alternatywami są moduły jak `pickle` (do obiektów Pythona), `json` (do danych typu JSON) czy `csv` (do plików CSV). Implementacja zapisu zmieniała się, ale design Pythona zachowuje prostotę operacji z plikami.

## Zobacz również:
- [Dokumentacja Pythona na temat I/O](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Moduł `json`](https://docs.python.org/3/library/json.html)
- [Moduł `csv`](https://docs.python.org/3/library/csv.html)
- [Praca z plikami w Pythonie - tutorial](https://realpython.com/read-write-files-python/)
