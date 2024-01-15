---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach, coraz więcej danych jest dostępnych w formie tekstowej. Dzięki wydobyciu podciągów tekstu, jesteśmy w stanie łatwiej i szybciej przetwarzać i analizować te dane, co może być niezbędne w różnych zastosowaniach, od analizy danych po automatyzację procesów biznesowych.

## Jak to zrobić

W Pythonie istnieją różne metody i funkcje pozwalające na wydobycie podciągów tekstu z ciągów znaków. Przykładowym sposobem jest użycie metody `slice()`, która pozwala na wybranie wybranego fragmentu tekstu, korzystając z indeksów.

```python
text = "To jest przykładowy tekst."
substring = text[8:19]

print(substring)

# Output: przykładowy
```

Można również użyć funkcji `find()`, aby zlokalizować indeks pierwszego wystąpienia danego podciągu w tekście, a następnie wykorzystać indeksy, aby wybrać odpowiedni fragment tekstu.

```python
text = "To jest przykładowy tekst."
substring = text[text.find("przykładowy"):text.find("tekst")]

print(substring)

# Output: przykładowy
```

Inną przydatną funkcją jest metoda `split()`, która dzieli dany tekst na podciągi, korzystając z określonego separatora. Może to być przydatne np. przy przetwarzaniu danych z plików CSV.

```python
text = "Jan,Alicja,Mateusz,Kasia"
names = text.split(",")

print(names)

# Output: ['Jan', 'Alicja', 'Mateusz', 'Kasia']
```

## W czym tkwi sedno

Podczas wydobywania podciągów, ważne jest, aby wiedzieć jaką metodę i jakie parametry wybrać w zależności od danego zastosowania. Możliwość precyzyjnego określenia indeksów czy wykorzystania funkcji `find()` czy `split()` może znacznie ułatwić przetwarzanie tekstów.

## Zobacz też

- [Oficjalna dokumentacja Pythona](https://docs.python.org/pl/3/library/stdtypes.html#common-sequence-operations)
- [Tutorial wideo o wydobyciu podciągów w Pythonie](https://www.youtube.com/watch?v=JaUJzj7Gids)
- [Pytania i odpowiedzi na Stack Overflow dotyczące wydobycia podciągów](https://stackoverflow.com/questions/tagged/python+substring)