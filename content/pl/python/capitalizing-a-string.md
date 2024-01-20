---
title:                "Zamiana tekstu na wielkie litery"
html_title:           "Python: Zamiana tekstu na wielkie litery"
simple_title:         "Zamiana tekstu na wielkie litery"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Za dużymi literami w programowaniu rozumiemy proces zamiany małych liter na duże w napisie. Programiści korzystają z tej funkcji, głównie do ułatwienia odczytywania tekstu lub wprowadzenia standardów programistycznych.

## Jak to zrobić:

Oto jak to zrobić w Pythonie:
```Python
# Przykład 1: Capitalize() funkcja
napis = 'witaj, świecie'
print(napis.capitalize())

# Przykład 2: Title() funkcja
napis = 'witaj, świecie'
print(napis.title())
```

Pierwszy kawałek kodu zwróci: 'Witaj, świecie'. Natomiast drugi: 'Witaj, Świecie'.

## Deep Dive:

Zastosowanie powyższych funkcji ma swoje genezy w starszych językach programowania, takich jak C, które nie miały wbudowanych funkcji do tej zmiany. Python dostarcza wiele ułatwień typu `capitalize()` i `title()`, które zwiększają wydajność i czytelność kodu.

Alternatywą jest używanie metody `upper()`, ale stosuje ona kapitalizację do całego ciągu, co nie zawsze może być pożądane.

Szczegóły implementacji zależą od specyficznej implementacji Pythona. Ale na ogół polega to na iteracji przez ciąg i zamianie każdego znaku na jego równoważnik wielką literą.

## Zobacz też:

Dla dalszej lektury i nauki, sprawdź te zasoby:

1. [Python String capitalize()](https://www.programiz.com/python-programming/methods/string/capitalize)
2. [Python String title()](https://www.w3schools.com/python/ref_string_title.asp)