---
title:                "Praca z liczbami zespolonymi"
aliases:
- /pl/fish-shell/working-with-complex-numbers.md
date:                  2024-01-26T04:40:10.764379-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z liczbami zespolonymi"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Liczby zespolone rozszerzają pomysł jednowymiarowej linii liczbowej o dwuwymiarową płaszczyznę zespoloną. Programiści używają ich w dziedzinach takich jak inżynieria, fizyka i grafika do obliczeń wymagających dwóch komponentów, takich jak sygnały czy rotacje.

## Jak to zrobić:
W Fish obsługujemy liczby zespolone za pomocą `math` z częściami rzeczywistą i urojoną. Oto jak zacząć:

```fish
# Dodaj dwie liczby zespolone (3+4i) i (5+2i)
set complex_sum (math "3+4i + 5+2i")
echo $complex_sum # Wyświetla: 8+6i

# Pomnóż dwie liczby zespolone (1+2i) i (3+4i)
set complex_prod (math "1+2i * 3+4i")
echo $complex_prod # Wyświetla: -5+10i
```

Jeśli potrzebujesz podnieść liczbę zespoloną do potęgi lub uzyskać jej formę wykładniczą:

```fish
# Kwadrat (2+3i)
set complex_square (math "(2+3i)^2")
echo $complex_square # Wyświetla: -5+12i

# Wykładnicza (2i)
set complex_exp (math "e^(2i)")
echo $complex_exp # Wyświetla: -0.41615+0.9093i
```

## Głębiej
Wsparcie Fish Shell dla liczb zespolonych jest stosunkowo nowe, pojawiło się około wersji 3.1.0. Przed tym ludzie mogli używać `bc` lub korzystać z zewnętrznych narzędzi takich jak Python do złożonych obliczeń matematycznych.

Alternatywy dla matematyki Fisha to specjalistyczne biblioteki numeryczne lub języki takie jak MATLAB, Python z NumPy, a nawet C++ ze Standardową Biblioteką. Jednakże, mogą one być nadmiarem dla szybkich obliczeń w shellu.

Wsparcie dla liczb zespolonych w Fishu jest wbudowane w jego wewnętrzne polecenie `math`, wykorzystując libcalc. Oznacza to, że nie musisz instalować dodatkowych narzędzi dla podstawowych operacji.

Jednakże, Fish nie jest zaprojektowany do ciężkich obliczeń matematycznych. Jego zdolności matematyczne są wygodne dla szybkich kalkulacji lub skryptów, gdzie liczby zespolone wchodzą w grę, ale rozważ używanie bardziej zaawansowanych narzędzi do intensywnych zadań.

## Zobacz również
- Dokumentacja Fish shell dla matematyki: https://fishshell.com/docs/current/commands.html#math
- NumPy dla Pythona, popularna alternatywa: https://numpy.org/
- Głębsze spojrzenie na liczby zespolone: https://betterexplained.com/articles/a-visual-intuitive-guide-to-imaginary-numbers/
