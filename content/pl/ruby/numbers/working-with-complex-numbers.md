---
title:                "Praca z liczbami zespolonymi"
aliases: - /pl/ruby/working-with-complex-numbers.md
date:                  2024-01-26T04:45:33.215638-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z liczbami zespolonymi"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Liczby zespolone, składające się z części rzeczywistej i urojonej (jak 3+4i), są podstawą w inżynierii i fizyce. Programiści pracują z nimi przy symulacjach, przetwarzaniu sygnałów i rozwiązywaniu równań, które nie współgrają tylko z liczbami rzeczywistymi.

## Jak to zrobić:
Ruby umożliwia łatwą pracę z liczbami zespolonymi. Możesz tworzyć je i manipulować nimi za pomocą klasy Complex:

```ruby
require 'complex'

# Tworzenie liczb zespolonych
c1 = Complex(3, 4)
c2 = Complex('2+5i')

# Podstawowe operacje
suma = c1 + c2               # => (5.0+9.0i)
różnica = c1 - c2            # => (1.0-1.0i)
iloczyn = c1 * c2            # => (-14.0+23.0i)
iloraz = c1 / c2             # => (0.896551724137931+0.03448275862068961i)

# Sprzężenie, moduł i faza
sprzężenie = c1.conjugate    # => (3.0-4.0i)
moduł = c1.abs                # => 5.0
faza = c1.phase               # Math.atan2(4, 3) => 0.9272952180016122 radiany

# Metody specyficzne dla liczb zespolonych
biegunowe = c1.polar          # => [5.0, 0.9272952180016122]
prostokątne = c1.rect         # => [3.0, 4.0]
```

## Pogłębiona analiza
Liczby zespolone nie są nowością – istnieją od XVI wieku, rozwiązując równania bez rzeczywistych rozwiązań. Pomijając matematykę, obliczeniowo klasa Complex w Ruby wykonuje ciężką pracę, wspierana przez moduł Math dla funkcji trygonometrycznych i transcendentalnych.

Wcześniejsze języki programowania wymagały ręcznego obsługiwania części rzeczywistych i urojonych. Niektóre, takie jak Fortran i C++, poświęcają specjalne biblioteki arytmetyce zespolonej.

Podejście Ruby'ego zakłada wbudowane wsparcie dla liczb zespolonych w jego składni, co zwalnia Cię od konieczności ponownego wynajdywania koła. Za kulisami klasa Complex zajmuje się matematyką, podczas gdy Ruby dba o interakcje obiektów.

## Zobacz też
- Dokumentacja Ruby na temat klasy Complex: [https://ruby-doc.org/core/Complex.html](https://ruby-doc.org/core/Complex.html)
- Podejście MathWorld do liczb zespolonych: [http://mathworld.wolfram.com/ComplexNumber.html](http://mathworld.wolfram.com/ComplexNumber.html)
- Wizualne wprowadzenie do liczb zespolonych i dlaczego są przydatne: [https://www.youtube.com/watch?v=5PcpBw5Hbwo](https://www.youtube.com/watch?v=5PcpBw5Hbwo)
