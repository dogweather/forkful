---
title:                "Bash: Generowanie losowych liczb"
programming_language: "Bash"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest nie tylko przydatne w programowaniu, ale także w różnych aspektach życia. Możesz użyć ich do stworzenia losowych haseł, wybierania losowych zwycięzców w konkursach, losowego generowania danych do testowania aplikacji i wiele więcej. Znajomość generowania losowych liczb jest niezbędna dla każdego programisty.

## Jak to zrobić

```Bash
# Generowanie losowej liczby z przedziału od 1 do 10
echo $((1 + RANDOM % 10))

# Generowanie losowego ciągu znaków o długości 10
cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 10 | head -n 1
```

Powyższe przykłady przedstawiają dwa sposoby generowania losowych liczb i znaków w języku Bash. Pierwszy wykorzystuje zmienną specjalną RANDOM, która zwraca losową liczbę. Musimy jedynie podać górny zakres, od którego chcemy generować liczby. Aby wygenerować losowy ciąg znaków, używamy komendy `tr`, która usuwa wszystkie znaki spoza wybranego przez nas zakresu, a następnie wykorzystujemy polecenie `fold` do sformatowania znaków w rzędzie i ograniczenia ich do określonej długości.

## Głębszy zanurzenie

Generowanie losowych liczb może wydawać się prostym zadaniem, ale istnieje wiele metod i algorytmów, które mogą zostać zastosowane. W przypadku zmiennej RANDOM, liczby generowane są w oparciu o tzw. generator liczb pseudolosowych. Oznacza to, że w rzeczywistości nie są one całkowicie losowe, tylko oparte na zadanym seedzie lub ziarnie. Dzięki temu możemy uzyskać te same wyniki przy każdym wywołaniu skryptu. W przypadku wykorzystania komendy `tr` musimy dokładnie wybrać znaki, których chcemy użyć, aby uniknąć wygenerowania podobnych lub jednakowych ciągów znaków.

## Zobacz także

- [Tutorial Bash: Generowanie losowych liczb](https://www.shell-tips.com/bash/random/)
- [Bash Hackers Wiki - Randomization](https://wiki.bash-hackers.org/scripts/randomization)
- [Generowanie losowej liczby w programowaniu w Bash](https://techinpolska.pl/bash/103-generowanie-losowej-liczby-w-programowaniu-w-bash)