---
title:                "Fish Shell: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Dlaczego

Generowanie losowych liczb może być przydatne w wielu różnych przypadkach, na przykład przy tworzeniu testów jednostkowych lub symulacji. W tym wpisie dowiesz się, jak można wykorzystać wbudowane funkcje w powłoce Fish Shell do generowania losowych liczb.

# Jak to zrobić


Fish Shell udostępnia kilka funkcji do generowania liczb losowych, a są to:

- $RANDOM - wewnętrzna zmienna, która przechowuje losową liczbę całkowitą z zakresu od 0 do 32767 (wliczając obie te wartości).
- "fish_random" - funkcja, która zwraca losową liczbę całkowitą z zakresu od 0 do 132767 (wliczając obie te wartości).
- "fish_gonrand" - funkcja, która zwraca losową liczbę zmiennoprzecinkową z zakresu od 0 do 1 (wyciągając losowe bity z generatora liczby pseudo-losowej).

Aby wygenerować losową liczbę, należy użyć polecenia "echo", który wyświetli wynik funkcji lub zmiennej. Na przykład:

```
Fish Shell: echo $RANDOM
18681
```

Aby wygenerować 10 losowych liczb, można użyć pętli "for" i funkcji "fish_random":

```
Fish Shell: for i in (seq 1 10)
               echo (fish_random)
           end
11971
5014
24241
2447
7939
27316
21511
407
28423
16927
```

Możesz także ustawić zakres wyświetlanych liczb, np. od 1 do 100, poprzez dodanie właściwych parametrów do funkcji. Na przykład:

```
Fish Shell: for i in (seq 1 10)
               echo (fish_random 1 100)
           end
98
53
65
4
30
10
66
72
3
90
```

# Deep Dive

Generowanie liczb losowych jest trudnym zagadnieniem w informatyce. W Fish Shell wykorzystywany jest generator liczb pseudo-losowych - funkcja "fish_gonrand" używa algorytmu LTCG (Linear Congruential Generator), który jest ulepszoną wersją popularnego generatora mersenne twister.

Ważne jest, aby pamiętać, że generowane liczby są jedynie "pseudo-losowe", a więc nie są całkowicie losowe i mogą zawierać pewne korelacje lub powtórzenia. Jest to wystarczające dla większości zastosowań, ale jeśli potrzebujesz bardzo losowej liczby, warto skorzystać z zewnętrznych narzędzi lub biblioteki.

# Zobacz także

Jeśli chcesz dowiedzieć się więcej o generowaniu liczb losowych w powłoce Fish Shell, polecamy zapoznanie się z poniższymi źródłami:

- https://fishshell.com/docs/current/commands.html#random
- http://scipy.github.io/devdocs/reference/generated/scipy.random.Generator.random.html
- https://www.tutorialspoint.com/unix_commands/seq.htm
- https://janisz.github.io/unix/shell/perl/bash/2012/12/28/random-numbers-in-unix-strona-dygestrecie.html