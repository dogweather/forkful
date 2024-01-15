---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Python: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasem podczas programowania potrzebujemy ustalić długość ciągu znaków (string). Dowiedz się, jak to zrobić w Pythonie i dlaczego jest to przydatne.

## Jak to zrobić

```Python
string = "Hello World!"
print(len(string))

# Output:
# 12
```

W tym przykładzie tworzymy zmienną "string" zawierającą ciąg znaków "Hello World!". Następnie używamy wbudowanej funkcji "len()", która zwraca długość tego ciągu.

## Deep Dive

Funkcja "len()" w Pythonie zwraca liczbę elementów w obiekcie. W przypadku ciągu znaków, obiektem jest właśnie ten ciąg, więc funkcja zwraca liczbę znaków w ciągu.

Warto również wiedzieć, że funkcja "len()" może być używana nie tylko dla ciągów znaków, ale również dla innych typów danych, takich jak listy czy krotki.

## Zobacz również

- [Oficjalna dokumentacja Pythona o funkcji "len()"](https://docs.python.org/3/library/functions.html#len)
- [Kurs "Python dla początkujących" na Codecademy](https://www.codecademy.com/learn/learn-python)
- [Poradnik "Jak liczyć długość ciągu znaków w Pythonie"](https://realpython.com/python-string-length/)