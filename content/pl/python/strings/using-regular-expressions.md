---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:13.433636-07:00
description: "Wyra\u017Cenia regularne (regex) to wzorce s\u0142u\u017C\u0105ce do\
  \ wyszukiwania kombinacji znak\xF3w w ci\u0105gach tekstowych. Programi\u015Bci\
  \ wykorzystuj\u0105 je do wyszukiwania, edycji\u2026"
lastmod: '2024-03-13T22:44:34.938694-06:00'
model: gpt-4-0125-preview
summary: "Wyra\u017Cenia regularne (regex) to wzorce s\u0142u\u017C\u0105ce do wyszukiwania\
  \ kombinacji znak\xF3w w ci\u0105gach tekstowych. Programi\u015Bci wykorzystuj\u0105\
  \ je do wyszukiwania, edycji\u2026"
title: "Korzystanie z wyra\u017Ce\u0144 regularnych"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wyrażenia regularne (regex) to wzorce służące do wyszukiwania kombinacji znaków w ciągach tekstowych. Programiści wykorzystują je do wyszukiwania, edycji lub manipulowania tekstem na podstawie zdefiniowanych wzorców, co czyni je niezbędnymi do zadań takich jak walidacja danych, parsowanie czy transformacja.

## Jak używać:
Użycie regex w Pythonie wiąże się z modułem `re`, który dostarcza zestaw funkcji do przetwarzania tekstu przy użyciu wyrażeń regularnych.

### Podstawowe dopasowywanie wzorców
Aby wyszukać wzorzec w ciągu tekstowym, użyj `re.search()`. Zwraca on obiekt dopasowania, gdy wzorzec zostanie znaleziony, w przeciwnym razie `None`.
```python
import re

text = "Ucz się programowania w Pythonie"
match = re.search("Python", text)
if match:
    print("Wzorzec znaleziony!")
else:
    print("Wzorzec nie znaleziony.")
```
Wynik:
```
Wzorzec znaleziony!
```

### Kompilowanie wyrażeń regularnych
Dla powtórzonego użycia tego samego wzorca, najpierw skompiluj go za pomocą `re.compile()` dla lepszej wydajności.
```python
pattern = re.compile("Python")
match = pattern.search("Ucz się programowania w Pythonie")
if match:
    print("Skompilowany wzorzec znaleziony!")
```
Wynik:
```
Skompilowany wzorzec znaleziony!
```

### Dzielenie ciągów znaków
Aby podzielić ciąg znaków przy każdym dopasowaniu wzorca regex, użyj `re.split()`.
```python
result = re.split("\s", "Python jest fajny")
print(result)
```
Wynik:
```
['Python', 'jest', 'fajny']
```

### Znajdowanie wszystkich dopasowań
Aby znaleźć wszystkie niepokrywające się wystąpienia wzorca, użyj `re.findall()`.
```python
matches = re.findall("n", "Programowanie w Pythonie")
print(matches)
```
Wynik:
```
['n', 'n']
```

### Zastępowanie tekstu
Użyj `re.sub()`, aby zastąpić wystąpienia wzorca nowym ciągiem znaków.
```python
replaced_text = re.sub("fajny", "niesamowity", "Python jest fajny")
print(replaced_text)
```
Wynik:
```
Python jest niesamowity
```

### Biblioteki stron trzecich
Chociaż wbudowany moduł `re` Pythona jest potężny, biblioteki stron trzecich takie jak `regex` oferują więcej funkcji i lepszą wydajność. Aby użyć `regex`, zainstaluj go za pomocą pip (`pip install regex`) i zaimportuj w swoim kodzie.

```python
import regex

text = "Nauka Pythona 3.8"
match = regex.search(r"Python\s(\d+\.\d+)", text)
if match:
    print(f"Znaleziona wersja: {match.group(1)}")
```
Wynik:
```
Znaleziona wersja: 3.8
```
