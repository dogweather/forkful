---
title:                "Python: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Długość (ang. length) jest jedną z podstawowych operacji w programowaniu. Jest to bardzo przydatne narzędzie, które pozwala na sprawdzenie ile znaków zawiera dany ciąg tekstu. Dzięki temu możemy lepiej manipulować tekstem w naszym kodzie.

## Jak to zrobić

Aby obliczyć długość tekstu w języku Python, możemy skorzystać z funkcji `len()`. Spójrzmy na poniższy przykład:

```Python
imie = "Katarzyna"
print(len(imie))
```

**Output:** `9`

W tym przypadku, funkcja `len()` zwróciła wartość 9, ponieważ ciąg "Katarzyna" składa się z 9 znaków. Możemy również użyć tej funkcji do obliczenia długości listy lub innego obiektu:

```Python
liczby = [1, 2, 3, 4, 5]
print(len(liczby))
```

**Output:** `5`

Ważne jest również pamiętanie, że funkcja `len()` działa nie tylko na typach wbudowanych, ale również na naszych własnych zdefiniowanych klasach i obiektach.

## Deep Dive

W języku Python, długość nie jest zapisywana jako atrybut obiektu, a funkcja `len()` jest wywoływana za każdym razem, kiedy chcemy poznać długość danego obiektu. W przypadku ciągów tekstowych, funkcja ta działa na zasadzie iteracji, sprawdzając ilość znaków od pierwszego do ostatniego.

Ponadto, funkcja `len()` jest często wykorzystywana do pętli for w celu określenia ilości iteracji. Przykładowo, jeśli chcemy wykonać pewną czynność określoną ilość razy, możemy użyć pętli for wraz z funkcją `len()`:

```Python
for i in range(len(liczby)):
    print(i)
```

**Output:**
```1
2
3
4
5
```

## Zobacz również

- Dokumentacja języka Python na temat funkcji len(): https://docs.python.org/3/library/functions.html#len
- Poradnik na temat pracy z napisami w języku Python: https://realpython.com/python-strings/ 
- Oficjalny kurs języka Python: https://www.python.org/about/gettingstarted/