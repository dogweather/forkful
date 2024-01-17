---
title:                "Łączenie ciągów znaków"
html_title:           "Python: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Łączenie ciągów znaków, zwane również konkatenacją, jest podstawową czynnością w programowaniu, która polega na łączeniu kilku wyrazów lub zdań w jeden ciąg. Programiści często używają konkatenacji w celu tworzenia dynamicznych wiadomości oraz generowania wyjścia do użytkownika.

## Jak to zrobić:

```Python
imie = "Anna"
nazwisko = "Kowalska"
wiadomosc = "Witaj " + imie + " " + nazwisko + "!" 
print(wiadomosc)
```
```
Wynik:
Witaj Anna Kowalska!

Możesz również łączyć więcej niż dwa ciągi znaków, używając operatora `+`:
```Python
liczba_1 = "7"
liczba_2 = "3"
wynik = liczba_1 + liczba_2
print(wynik)
```
Wynik:
73

## Głębsza perspektywa:

Tradycyjnie konkatenacja została wprowadzona w języku programowania Fortran w 1957 roku. Obecnie jest szeroko stosowana w wielu językach programowania, w tym w Pythonie. Alternatywą dla konkatenacji jest używanie nawiasów w celu wyrażenia wielu ciągów w jednym wyrażeniu.

Możesz też użyć metody `.join()` w celu połączenia elementów listy w jeden ciąg znaków:
```Python
lista = ["AB", "CD", "EF"]
wynik = "-".join(lista)
print(wynik)
```
Wynik:
AB-CD-EF

Implementacja konkatenacji w Pythonie jest zoptymalizowana pod kątem wydajności, co oznacza, że łączenie większej liczby ciągów niż dwóch nie będzie powodować większej liczby operacji, a zatem będzie działać szybko i wydajnie.

## Zobacz też:

- Dokumentacja Pythona na temat konkatenacji: https://docs.python.org/3/library/stdtypes.html#string-methods
- Porównanie konkatenacji i użycia nawiasów: https://realpython.com/python-strings/