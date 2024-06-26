---
date: 2024-01-26 04:44:58.769828-07:00
description: "Jak: Python ma wbudowane wsparcie dla liczb z\u0142o\u017Conych. Oto\
  \ jak mo\u017Cesz si\u0119 nimi bawi\u0107."
lastmod: '2024-03-13T22:44:34.942906-06:00'
model: gpt-4-0125-preview
summary: "Python ma wbudowane wsparcie dla liczb z\u0142o\u017Conych."
title: Praca z liczbami zespolonymi
weight: 14
---

## Jak:
Python ma wbudowane wsparcie dla liczb złożonych. Oto jak możesz się nimi bawić:

```Python
# Tworzenie liczb złożonych
z = 4 + 5j
print(z)  # Wyjście: (4+5j)

# Dostęp do części rzeczywistej i urojonej
print(z.real)  # Wyjście: 4.0
print(z.imag)  # Wyjście: 5.0

# Arytmetyka złożona
w = 1 - 2j
print(z + w)  # Wyjście: (5+3j)
print(z - w)  # Wyjście: (3+7j)
print(z * w)  # Wyjście: (14+2j)
print(z / w)  # Wyjście: (-3.6+1.2j)

# Moduł (wartość bezwzględna)
print(abs(z))  # Wyjście: 6.4031242374328485

# Sprzężenie liczby złożonej
print(z.conjugate())  # Wyjście: (4-5j)
```

## Wprowadzenie
Liczby złożone zostały po raz pierwszy skonceptualizowane przez Gerolamo Cardano w XVI wieku. Python, wśród innych języków programowania, traktuje liczby złożone jako obiekty pierwszoklasowe. Oznacza to, że są one wbudowane w język, z łatwo dostępnymi funkcjami, co eliminuje potrzebę importowania zewnętrznych bibliotek do podstawowych operacji.

Jednak dla zaawansowanych obliczeń numerycznych, Python posiada bibliotekę o nazwie `cmath`, która jest przeznaczona specjalnie dla liczb złożonych. Zawiera ona dodatkowe funkcje takie jak `exp`, `log` i operacje trygonometryczne.

Gdy Python nie wystarcza, możesz sięgnąć po biblioteki takie jak NumPy, szczególnie dla operacji na tablicach zawierających liczby złożone. NumPy zapewnia zoptymalizowane i zwektoryzowane operacje, które są kluczowe dla wydajności w obliczeniach numerycznych.

## Zobacz także
Sprawdź te zasoby, aby dowiedzieć się więcej:

- Oficjalna dokumentacja Pythona na temat liczb złożonych: https://docs.python.org/3/library/stdtypes.html#typesnumeric
- Dokumentacja modułu `cmath`: https://docs.python.org/3/library/cmath.html
- NumPy do obsługi tablic liczb złożonych: https://numpy.org/doc/stable/user/absolute_beginners.html#the-basics
