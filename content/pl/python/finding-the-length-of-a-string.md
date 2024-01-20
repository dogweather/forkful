---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?

Znajdowanie długości łańcucha to proces, który pozwala programistom dowiedzieć się, ile znaków znajduje się w danym łańcuchu. Programiści robią to, aby kontrolować przepływ danych w swoim kodzie.

## Jak to zrobić:

W Pythonie możemy używać wbudowanej funkcji `len()`, aby znaleźć długość łańcucha. 

```Python
napis = "Programowanie w Pythonie"
print(len(napis))
```
Gdy uruchomisz powyższy kod, otrzymasz wynik `24`, co jest długością łańcucha "Programowanie w Pythonie".

## W głąb tematu:

Python, jako język o wysokim poziomie, daje nam prostotę korzystania z funkcji `len()`. W historii jednak, inne niż Python języki (takie jak C), wymagały od programistów utworzenia dedykowanej funkcji do zliczania znaków w łańcuchu.

Alternatywą dla `len()` jest metoda `.__len__ ()`. Chociaż nie zaleca się jej używać w codziennej pracy z Pythonem ze względu na różnice w składni i mniejszą czytelność, jej działanie jest takie samo:

```Python
napis = "Programowanie w Pythonie"
print(napis.__len__())
```

Python implementuje `len()` w taki sposób, że w rzeczywistości zwraca wynik metody `.__len__() ` obiektu. Ale zasada 'Explicit is better than implicit' z `Zen of Python` przemawia za bardziej bezpośrednim i czytelnym podejściem, jakim jest korzystanie z `len()`.

## Zobacz też:

Więcej informacji odnośnie operacji na łańcuchach w Pythonie można znaleźć na Oficjalnej Stronie Dokumentacji:
- Dokumentacja Python 3.9.4: [Operacje na łańcuchach](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- Przewodnik Python: [Manipulacja łańcuchami](https://docs.python.org/3/tutorial/introduction.html#strings)