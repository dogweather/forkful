---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Usuwanie Znaków Odpowiadających Wzorcowi w Pythonie

## Czym i Dlaczego?

Usuwanie znaków, które pasują do określonego wzorca, to jedno z podstawowych zadań programisty - pomaga w czyszczeniu danych wejściowych, manipulowaniu tekstami i obsłudze różnych przypadków. Narzędzia języka Python umożliwiają wykonanie tej pracy efektywnie i łatwo.

## Jak to Zrobić:

Kod Pythona do usuwania znaków pasujących do wzorca jest prosty. Poniżej znajdują się przykładowe zastosowania:

```Python
import re 

# Usuń wszystkie cyfry z tekstu
tekst = re.sub('\d', '', 'abc123')
print(tekst)  # output: 'abc'

# Usuń wszystkie litery
tekst = re.sub('[a-zA-Z]', '', 'abc123')
print(tekst)  # output: '123'
```
Powyższy kod używa modułu 're' i metody 'sub' do wyszukiwania wzorców i ich zastępowania. 

## Głębsze Zrozumienie:

Historia wyrażeń regularnych, które są silnym narzędziem do manipulowania wzorcami, ma korzenie w teorii języków formalnych i automatach. Alternatywną metodą manipulowania tekstem w Pythonie mogą być listy składane lub inne techniki zbiorów.

Implementacja usuwania znaków pasujących do określonego wzorca jest efektywna dzięki silnym mechanizmom Pythona do obsługi wyrażeń regularnych i manipulacji ciągami. W Pythonie każdy ciąg znaków zawsze jest obiektem, co zapewnia szybkość i wydajność.

## Zobacz Również:

1. Dokumentacja modułu 're' w Pythonie: [https://docs.python.org/3/library/re.html](https://docs.python.org/3/library/re.html)
2. Python - Manipulowanie Tekstem: [https://docs.python.org/3/tutorial/inputoutput.html#fancier-output-formatting](https://docs.python.org/3/tutorial/inputoutput.html#fancier-output-formatting)
3. Wyrażenia Regularne (Regex) w Pythonie: [https://realpython.com/regex-python/](https://realpython.com/regex-python/)