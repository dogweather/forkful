---
title:                "Python: Usuwanie znaków odpowiadających wzorcowi"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że w trakcie programowania musimy poradzić sobie z nierządnie sformatowanymi danymi. Jeden ze sposobów na poradzenie sobie z takim problemem jest usuwanie znaków pasujących do pewnego wzorca. W tym artykule pokażę, jak to zrobić w języku Python.

## Jak to zrobić

Ten problem można rozwiązać za pomocą metody ```strip()``` w języku Python. Ta metoda usuwa wszystkie znaki, które pasują do podanego wzorca z końca lub początku stringa. Poniżej przedstawiam przykład, jak usunąć wszystkie znaki spacji z końca stringa:

```Python
text = "Przykładowy tekst do usunięcia "
print(text.strip())
```

Output:

```Przykładowy tekst do usunięcia```

Aby usunąć znaki pasujące do wzorca z dowolnego miejsca w stringu, należy użyć metody ```replace()```. Poniżej przedstawiam przykład, jak usunąć wszystkie litery 'a' z dowolnego miejsca w stringu:

```Python
text = "Przykładowy tekst do ususnięcia"
print(text.replace('a', ''))
```

Output:

```Przykłdowy tekst do uśnięci```

## Głębszy rozkład

Język Python oferuje wiele innych możliwości manipulacji stringami. Możesz również użyć operatorów logicznych, wyrażeń regularnych lub popularnej biblioteki ```re``` do usuwania znaków pasujących do określonego wzorca.

## Zobacz również

[Zręczne dzielenie tekstu na fragmenty w Pythonie](https://www.programiz.com/python-programming/methods/string/split)
[Pomocne funkcje do manipulowania stringami w języku Python](https://www.geeksforgeeks.org/python-output-formatting/)
[Manipulacja stringami przy użyciu modułu ```re``` w języku Python](https://www.tutorialspoint.com/python/string_re_replace.htm)