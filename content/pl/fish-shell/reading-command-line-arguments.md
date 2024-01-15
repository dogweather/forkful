---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "Fish Shell: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś fanem wygodnego i szybkiego interfejsu wiersza poleceń, to ta funkcjonalność jest dla Ciebie! Pozwala ona na łatwą i szybką manipulację wartościami wprowadzonymi przez użytkownika, co jest niezbędne w wielu skryptach i programach. 

## Jak to zrobić

```Fish Shell``` to potężne narzędzie do pracy z wierszem poleceń, a jego obsługa jest niezwykle prosta. Aby odczytać argumenty wprowadzone przez użytkownika, wystarczy użyć zmiennej specjalnej ```$argv```, która przechowuje tablicę z podanymi wartościami. Na przykład, jeśli uruchomimy skrypt z argumentami "Hello" i "World", poniższy kod zwróci wynik "Hello World":

```
set nazwa_wyjscia $argv[1]
echo $nazwa_wyjscia $argv[2]
```

Jeśli chcesz uzyskać dostęp do pojedynczego argumentu, możesz użyć specjalnej składni ```$argv[liczba]```, gdzie ```liczba``` oznacza pozycję argumentu. Pierwszy argument zawsze jest przechowywany pod indeksem 1, drugi pod indeksem 2, i tak dalej. 

## Głębsze zanurzenie

Możesz także wykorzystać dodatkowe zmienne specjalne, takie jak ```$argc```, która przechowuje liczbę podanych argumentów lub ```$argv0```, która przechowuje nazwę wywołanego pliku. Warto także wspomnieć, że zmienne specjalne mogą być używane z dowolnym innym poleceniem, dzięki czemu dają wiele możliwości do manipulacji danymi wprowadzonymi przez użytkownika. 

## Zobacz także

- Oficjalna dokumentacja ```Fish Shell```: https://fishshell.com/docs/current/cmds/set.html
- Bitbucket Repozytorium ```Fish Shell```: https://bitbucket.org/fish-shell/fish-shell/src/master/
- Przewodnik po ```Fish Shell```: https://gist.github.com/erichs/8551261