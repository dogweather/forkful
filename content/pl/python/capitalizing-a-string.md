---
title:    "Python: Zapisywanie tekstu wielką literą"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Dlaczego

Witaj Pythonie! Może zastanawiasz się dlaczego ktoś chciałby zmienić wielkość liter w ciągu znaków. Cóż, powody mogą być różne - może chcesz zrobić tekst wyraźniejszym, lub dopasować do konwencji w swoim kodzie. Bez względu na przyczynę, w tym artykule pokażę Ci, jak w prosty sposób zmienić wielkość liter w Pythonie.

## Jak to zrobić

```python
# Definiowanie ciągu znaków
tekst = "witaj polska społeczność pythona!"

# Użycie metody capitalize()
tekst = tekst.capitalize()

# Wyświetlenie wyniku
print(tekst)

# Output: Witaj polska społeczność pythona!
```

Wyjaśnienie kodu: Najpierw definiujemy ciąg znaków, a następnie używamy metody 'capitalize()' na naszej zmiennej 'tekst'. Metoda ta zmienia pierwszą literę ciągu na dużą, a resztę na małe. Na koniec wyświetlamy zmieniony tekst za pomocą funkcji 'print()'.

Innym sposobem jest użycie metody 'title()', która zamieni wszystkie pierwsze litery poszczególnych słów na duże.

```python
# Definiowanie ciągu znaków
tekst = "witaj polska społeczność pythona!"

# Użycie metody title()
tekst = tekst.title()

# Wyświetlenie wyniku
print(tekst)

# Output: Witaj Polska Społeczność Pythona!
```

Możesz również zmienić wielkość liter w całym tekście na małe lub duże za pomocą metod 'lower()' i 'upper()'.

```python
# Definiowanie ciągu znaków
tekst = "witaj polska społeczność pythona!"

# Użycie metod lower() i upper()
tekst_lower = tekst.lower()
tekst_upper = tekst.upper()

# Wyświetlenie wyników
print(tekst_lower)
print(tekst_upper)

# Output: witaj polska społeczność pythona!
#         WITAJ POLSKA SPOŁECZNOŚĆ PYTHONA!
```

Warto również zauważyć, że metody 'capitalize()' i 'title()' działają tylko na pierwszej literze w ciągu znaków, a metody 'lower()' i 'upper()' zmieniają wszystkie litery w tekście.

## Głębszy zanurzenie

W Pythonie istnieje kilka innych metod do zmiany wielkości liter w ciągu znaków. Jest to ze względu na to, że Python traktuje ciągi znaków jako niemodyfikowalne obiekty, więc nie można bezpośrednio zmienić ich wartości.

Metoda 'swapcase()' zamienia duże litery na małe i odwrotnie.

```python
# Definiowanie ciągu znaków
tekst = "WITAJ POLSKA SPOŁECZNOŚĆ PYTHONA!"

# Użycie metody swapcase()
tekst = tekst.swapcase()

# Wyświetlenie wyniku
print(tekst)

# Output: witaj polska społeczność pythona!
```

Inne przydatne metody to 'strip()', która usuwa białe znaki na początku i końcu ciągu oraz 'find()', która znajduje pozycję podanego tekstu w ciągu.

## Zobacz także

- [Dokumentacja Pythona o ciągach znaków](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Tutorial na YouTube o metodach do manipulacji ciągami znaków w Pythonie](https://www.youtube.com/watch?v=j-StCG9gGW8)
- [Kurs języka Python na Codecademy](https://www.codecademy.com/learn/learn-python)