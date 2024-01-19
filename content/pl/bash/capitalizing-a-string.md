---
title:                "Zamiana liter w łańcuchu na wielkie"
html_title:           "Bash: Zamiana liter w łańcuchu na wielkie"
simple_title:         "Zamiana liter w łańcuchu na wielkie"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

---

## Co i dlaczego?

Zmiana literek na wielkie w łańcuchach znaków polega na przekształceniu wszystkich liter małych na duże. Programiści robią to by ułatwić czytelność tekstu lub zapewnić spójność danych.

## Jak to zrobić?

Kod w Bashu, który zmienia małe litery na wielkie w podanym łańcuchu, wygląda tak:

```Bash
string="tekst do zamiany"
echo "${string^^}"
```

Gdy uruchomisz ten skrypt, na ekranie pojawi się:

```Bash
TEKST DO ZAMIANY
```

## Dogłębna analiza

1) Kontekst historyczny: Bash wprowadził możliwość zmiany liter w wersji 4.0, wydanej w 2009 roku.
2) Alternatywy: Inne języki programowania, takie jak Python czy JavaScript, mają wbudowane metody służące do tego celu, np. `.toUpperCase()` w JavaScript i `.upper()` w Python.
3) Szczegóły implementacji: Bash zamienia małe litery na wielkie, przechodząc przez każdy znak ciągu i sprawdzając, czy jest on literą. Jeśli jest literą, Bash zamienia ją na odpowiednik dużych liter.

## Zobacz również

Do dalszych źródeł związanych z tym tematem należą:

- Dokumentacji Bash (obsługa łańcucha znaków): https://tldp.org/LDP/abs/html/string-manipulation.html
- Poradnik o obsłudze liter wielkich i małych w Bashu: http://www.linuxtopia.org/online_books/scripting_guide/bash_string_manipulation.html
- Wyjaśnienie, dlaczego i kiedy zmienić łańcuch na wielkie litery: https://unix.stackexchange.com/questions/605331/why-and-when-to-convert-string-to-uppercase