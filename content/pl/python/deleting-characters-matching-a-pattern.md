---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "Python: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Usuwanie znaków odpowiadających wzorcowi jest operacją, którą programiści często wykonują podczas przetwarzania tekstu za pomocą języka Python. Polega ona na usunięciu wszystkich wystąpień określonego wzorca z ciągu znaków. Jest to przydatne, gdy chcemy pozbyć się niepożądanych znaków lub zamienić je na inne.

## Jak wykonać:
```Python
# Przykładowe wejście:
s = "Hello world!"
pattern = "o"
# Oczekiwane wyjście:
Hll wrld!
```

## Głębsze spojrzenie:
Usuwanie znaków odpowiadających wzorcowi jest powszechną operacją w przetwarzaniu tekstu. Początkowo wykorzystywano do tego celu pętle for lub metody takie jak replace(). Jednak dzięki wyrażeniom regularnym, które są specjalnym typem wzorca, możemy wykonać tę operację szybciej i bardziej wydajnie. Istnieją również inne metody, takie jak list comprehension i funkcja filter(), które umożliwiają usuwanie znaków zgodnych z określonym warunkiem.

## Zobacz także:
Artykuł o wyrażeniach regularnych w języku Python: https://realpython.com/regex-python/

Dokumentacja Pythona dotycząca pracy z tekstem: https://docs.python.org/3/library/string.html