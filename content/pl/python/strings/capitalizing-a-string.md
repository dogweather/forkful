---
aliases:
- /pl/python/capitalizing-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:20.835334-07:00
description: "Kapitalizacja ci\u0105gu znak\xF3w oznacza przekszta\u0142cenie pierwszego\
  \ znaku ci\u0105gu na wielk\u0105 liter\u0119, a reszt\u0119 na ma\u0142e litery.\
  \ Operacja ta jest powszechnie\u2026"
lastmod: 2024-02-18 23:08:49.188406
model: gpt-4-0125-preview
summary: "Kapitalizacja ci\u0105gu znak\xF3w oznacza przekszta\u0142cenie pierwszego\
  \ znaku ci\u0105gu na wielk\u0105 liter\u0119, a reszt\u0119 na ma\u0142e litery.\
  \ Operacja ta jest powszechnie\u2026"
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?
Kapitalizacja ciągu znaków oznacza przekształcenie pierwszego znaku ciągu na wielką literę, a resztę na małe litery. Operacja ta jest powszechnie stosowana w przetwarzaniu danych w celu normalizacji danych wejściowych lub poprawy czytelności tytułów, nazwisk itp.

## Jak to zrobić:

### Korzystając z wbudowanej metody Pythona:
Python posiada wbudowaną metodę `.capitalize()` dla ciągów znaków, która pozwala łatwo wykonać to zadanie.

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**Wynik:**
```
Hello world
```

### Obsługa wielu słów:
W przypadkach, gdy chcesz, aby każde słowo w ciągu rozpoczynało się od wielkiej litery (tak jak w tytułach), można zastosować metodę `.title()`.

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**Wynik:**
```
Python Programming Essentials
```

### Korzystanie z bibliotek zewnętrznych:
Chociaż standardowa biblioteka Pythona jest wyposażona do podstawowej kapitalizacji ciągów znaków, biblioteki takie jak `textblob` mogą oferować bardziej subtelne sterowanie, szczególnie przy przetwarzaniu języka naturalnego.

Najpierw upewnij się, że masz zainstalowany `textblob`:
```bash
pip install textblob
```

Następnie użyj go do kapitalizacji ciągów znaków, pamiętając, że kapitalizacja w `textblob` może działać inaczej w zależności od kontekstu użycia:

```python
from textblob import TextBlob

my_sentence = "this is a test sentence"
blob = TextBlob(my_sentence)
capitalized_blob = TextBlob(blob.string.capitalize())
print(capitalized_blob)
```
**Wynik:**
```
This is a test sentence
```

Pamiętaj, że choć metody `capitalize()` i `title()` są uniwersalnie przydatne, korzystanie z bibliotek takich jak `textblob` może zapewnić dodatkową elastyczność dla konkretnych zastosowań.
