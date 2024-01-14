---
title:                "Python: Wyciąganie podłańcuchów"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu musimy pracować z łańcuchami znaków, czyli ciągami liter, cyfr i symboli. Jedną z przydatnych technik jest wyodrębnianie podłańcuchów, czyli fragmentów danego łańcucha. W tym artykule dowiesz się dlaczego warto nauczyć się tej techniki i jak jej używać w języku Python.

## Jak to zrobić

Wyodrębnianie podłańcuchów w Pythonie jest bardzo proste i korzysta z funkcji `str[start:stop]`. Start oznacza indeks, od którego ma zacząć się wyodrębnianie, a stop to indeks, na którym ma się skończyć. Pamiętaj, że indeksy zaczynają się od 0. Przykładowy kod wykorzystujący tę funkcję wyglądałby następująco:

```Python
str = "Python jest wspaniały językiem programowania"
print(str[0:6]) # wyświetli "Python"
print(str[14:19]) # wyświetli "wspan"
```

Możemy również wykorzystać przestawienie indeksów, na przykład `str[:5]` oznacza, że chcemy wyodrębnić podłańcuch od początku do indeksu 5 (nie włącznie). Natomiast `str[10:]` oznacza, że chcemy wyodrębnić podłańcuch od indeksu 10 do końca łańcucha. Spróbuj samemu zmieniać wartości indeksów i sprawdzić, jakie podłańcuchy uzyskasz.

## Głębszy wgląd

W Pythonie możemy również wyodrębnić co drugi lub co trzeci znak z łańcucha. W tym celu należy dodać krok do naszej funkcji `str[start:stop:step]`. Na przykład, jeśli chcemy wyodrębnić co drugą literę z naszego łańcucha, możemy napisać `str[::2]`.

Ważną rzeczą do zapamiętania jest to, że funkcja `str[start:stop]` zwraca podłańcuch od indeksu start do indeksu stop-1. Dlatego też, jeśli chcemy uzyskać cały łańcuch, należy użyć wartości `str[:]`.

## Zobacz również

- [Dokumentacja Pythona na temat wyodrębniania podłańcuchów](https://docs.python.org/3.7/library/stdtypes.html#text-sequence-type-str)
- [Artykuł na temat wyodrębniania podłańcuchów w Pythonie](https://realpython.com/python-substrings/)
- [Praktyczne przykłady wykorzystania wyodrębniania podłańcuchów](https://www.programiz.com/python-programming/string-slicing)