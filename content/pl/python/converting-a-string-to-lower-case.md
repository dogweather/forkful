---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zmiana ciągu znaków (stringa) na małe litery to proces, w którym wszystkie litery w stringu są konwertowane na odpowiedniki w małych literach. Programiści robią to, aby uniknąć niepotrzebnych błędów i ułatwić sortowanie i porównywanie stringów. 

## Jak to zrobić:

Oto prosty przykład, jak w Pythonie można przekształcić ciąg znaków na małe litery:

```python
text = "CZEŚĆ, ŚWIECIE!"
lower_case_text = text.lower()
print(lower_case_text)
```

Po uruchomieniu tego kodu otrzymasz:

```python
'cześć, świecie!'
```

## Pogłębione spojrzenie:

1. **Kontekst historyczny**: W dawnych językach programowania, takich jak COBOL czy FORTRAN, nie było tak łatwo konwertować ciągu znaków na małe litery. W Pythonie jest to bardzo proste dzięki wbudowanej funkcji `.lower()`.

2. **Alternatywy**: Istnieją inne sposoby na konwertowanie stringów na małe litery w Pythonie, na przykład poprzez użycie list comprehension: 

    ```python
    text = "CZEŚĆ, ŚWIECIE!"
    lower_case_text = ''.join([char.lower() for char in text])
    print(lower_case_text)
    ```

    Ale najprostszym i najbardziej zalecanym sposobem jest użycie wbudowanej funkcji `.lower()`.

3. **Szczegóły implementacji**: Funkcja `.lower()` mapuje każdą dużą literę w stringu na jej małoliterowy odpowiednik. Działa to dla wszystkich znaków Unicode, które mają małe i duże wersje.

## Zobacz również:

Możesz dowiedzieć się więcej o manipulacji stringami w Pythonie, odwiedzając oficjalną dokumentację Pythona na temat ciągów znaków [tutaj](https://docs.python.org/pl/3.8/library/string.html). Jeśli jesteś początkujący, [ten tutorial](https://www.learnpython.org/en/Basic_String_Operations) nauczy Cię podstaw operacji na stringach.