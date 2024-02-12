---
title:                "Wycinanie podłańcuchów"
aliases:
- /pl/python/extracting-substrings/
date:                  2024-01-20T17:46:30.645907-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wycinanie podłańcuchów"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wyciąganie podłańcuchów to proces wzięcia kawałka tekstu z większego ciągu znaków. Programiści robią to, by operować na określonych fragmentach danych - czy to dla weryfikacji, wyszukiwania czy manipulacji tekstem.

## Jak to zrobić:
Wyciągając podłańcuch w Pythonie, często używamy indeksowania i wycinania (`slicing`). Oto kilka przykładów:

```python
text = "Programowanie to pasja!"

# Pierwsze pięć znaków
substr1 = text[:5]
print(substr1)  # Wypisze 'Progr'

# Od dziesiątego do piętnastego znaku
substr2 = text[10:15]
print(substr2)  # Wypisze 'anie '

# Ostatnie cztery znaki
substr3 = text[-4:]
print(substr3)  # Wypisze 'asja!'
```

## W głębi tematu:
W Pythonie, historia wycinania sięga wczesnych dni języka. Mechanizm ten pochodzi z koncepcji podobnych do tych w innych starszych językach jak Perl czy C, gdzie manipulacja ciągami znaków była powszechna.

Alternatywą do wycinania jest użycie metody `substring()`, która istnieje w niektórych innych językach, jak Java czy C#. W Pythonie jednak nie ma wbudowanej metody `substring()`, a wycinanie ('slicing') jest metodą idiomatyczną i preferowaną.

Warto też wspomnieć o wydajności. Python korzysta z obiektów `slice`, które mogą być użyte powtórnie; jest to szczególnie użyteczne, kiedy robimy tę samą operację wiele razy.

## Zobacz także:
- [Dokumentacja Python - Cięcia ciągów znaków](https://docs.python.org/3/tutorial/introduction.html#strings)
- [Python String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods) - dokumentacja zawierająca metody operujące na stringach.
- [Real Python - Guide to Slicing](https://realpython.com/lessons/indexing-and-slicing/) - głębokie zanurzenie w indeksowanie i wycinanie w Pythonie.
