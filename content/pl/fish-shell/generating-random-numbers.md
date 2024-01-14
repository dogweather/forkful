---
title:                "Fish Shell: Generowanie losowych liczb"
programming_language: "Fish Shell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb może być niezbędne w wielu projektach programistycznych. Może być wykorzystane do testowania kodu, tworzenia losowych unikalnych identyfikatorów czy symulacji zachowań losowych. W tym wpisie dowiesz się, jak wygenerować losowe liczby w powłoce Fish Shell.

## Jak to zrobić

Aby wygenerować losową liczbę w Fish Shell, wykorzystujemy wbudowaną funkcję `random`. Możemy wybrać, ile liczb chcemy wygenerować oraz zakres, w jakim mają się znajdować. Przykładowe użycie wygląda następująco:

```Fish Shell
set i 0
while test $i -lt 10
    random 1 100
    set i (math $i + 1)
end
```

Powyższy kod wygeneruje 10 liczb losowych z zakresu od 1 do 100. Wynik zostanie wyświetlony na ekranie w kolejnych liniach. Możemy również przypisać wynik do zmiennej, aby go wykorzystać w dalszych obliczeniach.

## Głębszy zanurzenie

Funkcja `random` wykorzystuje generator liczb pseudolosowych, który działa na podstawie "ziarna" (seed). W przypadku powtórnego uruchomienia kodu z takim samym ziarnem, wynik będzie taki sam. Aby uniknąć anulowania losowości, warto zmieniać ziarno przekazywane do funkcji. Możesz to zrobić, wykorzystując zmienną systemową `$RANDOM`, która jest aktualizowana po każdym wywołaniu funkcji `random`.

```Fish Shell
set i 0
while test $i -lt 10
    random $RANDOM 1 100
    set i (math $i + 1)
end
```

## Zobacz także

- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/cmds/random.html)
- [Wprowadzenie do powłoki Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Wprowadzenie do generowania liczb pseudolosowych](https://pl.wikipedia.org/wiki/Generator_liczb_pseudolosowych)