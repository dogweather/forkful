---
title:                "Wycinanie podciągów."
html_title:           "Python: Wycinanie podciągów."
simple_title:         "Wycinanie podciągów."
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/extracting-substrings.md"
---

{{< edit_this_page >}}

Czego i po co?

Wycinanie podłańcuchów, czyli wyłączanie fragmentów tekstu z całego ciągu znaków, jest jedną z powszechnie używanych operacji w programowaniu. Programiści używają go głównie w celu filtrowania i manipulacji tekstem, na przykład wyodrębniania konkretnych informacji z dłuższego ciągu znaków.

Jak to zrobić:

```Python
# Przykładowy ciąg znaków
text = 'Python jest wspaniałym językiem programowania'

# Wyodrębnienie podłańcucha z indeksem
print(text[0:6])  # Wynik: 'Python'

# Wyodrębnienie podłańcucha z wykorzystaniem funkcji split()
words = text.split(' ')
print(words[0])  # Wynik: 'Python'

# Wyodrębnienie podłańcucha z wykorzystaniem metody find()
index = text.find('językiem')
print(text[index:])  # Wynik: 'językiem programowania'
```

Głębszy zanurzenie:

Wycinanie podłańcuchów jest możliwe dzięki indeksowaniu i cięciu w Pythonie, co jest inspirowane podobnymi funkcjami w języku C. Istnieje wiele alternatywnych sposobów na wycinanie podłańcuchów, w tym wykorzystanie wyrażeń regularnych lub biblioteki string. Warto również pamiętać, że wycięte podłańcuchy są typem danych `str`, który można dalej manipulować.

Zobacz też:

- [Dokumentacja Python](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- [Tutorial wyciągania podłańcuchów](https://www.programiz.com/python-programming/string-slicing)
- [Poradnik wyrażeń regularnych](https://realpython.com/regex-python/)