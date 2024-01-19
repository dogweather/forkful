---
title:                "Łączenie ciągów znaków"
html_title:           "Arduino: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?

Łączenie ciągów znaków, czyli konkatenacja, to sposób na scalenie dwóch lub więcej napisów w jeden. Programiści robią to, aby manipulować i formatować dane tekstowe w łatwy i wydajny sposób.

## Jak to zrobić:

W Pythonie mamy kilka podstawowych technik do łączenia ciągów znaków.

1. Operator `+`:
```Python
napis1 = "Cześć"
napis2 = "Pythonie"
print(napis1 + ", " + napis2)
```
Na wyjściu otrzymasz:
```
Cześć, Pythonie
```

2. String Formatowanie:
```Python
print("{} jest super!".format("Python"))
```
Na wyjściu otrzymasz:
```
Python jest super!
```

3. f-strings (dostępne od Python 3.6):
```Python
wersja = 3.6
print(f"Używamy Pythona {wersja}")
```
Na wyjściu otrzymasz:
```
Używamy Pythona 3.6
```

## Deep Dive

Konkatenacja stringów to technika stosowana od początku istnienia języków programowania. Zasada jest prosta: skleić razem różne ciągi znaków. 

Ale w Pythonie, mamy więcej niż jedną metodę na łączenie ciągów. Operator `+` jest klasycznym sposobem, ale ma pewne ograniczenia. Nie łączy ciągów z innymi typami danych bez jawnego rzutowania.

Nowocześniejsze metody, takie jak f-strings, są bardziej elastyczne i pozwalają na proste formatowanie ciągów. F-strings są również wydajne pod względem pamięci i wydajności. 

Alternatywą dla wszystkich tych metod może być metoda `join()`. Jest to metoda bardziej Pythoniczna, czyli bardziej zgodna z filozofią Pythona, preferująca czytelność i prostotę.
 
```Python
lista_słów = ['Cześć', 'świecie', 'Pythona']
print(' '.join(lista_słów))
```
Na wyjściu otrzymasz:
```
Cześć świecie Pythona
```

## Zobacz też

Zaawansowane formatowanie ciągów, dokumentacja Pythona: https://docs.python.org/3/library/string.html#format-string-syntax

PEP 498 -- Literał formatujący ciągu znaków (f-strings) w Pythonie 3.6: https://peps.python.org/pep-0498/ 

Metoda `join()`, dokumentacja Pythona: https://docs.python.org/3/library/stdtypes.html#str.join