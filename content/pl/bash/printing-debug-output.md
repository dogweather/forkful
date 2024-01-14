---
title:    "Bash: Wydrukuj wyjście debugowania"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Dlaczego

W programowaniu bardzo często spotykamy się z błędami i problemami, które trzeba rozwiązać. W takiej sytuacji bardzo pomocne jest dodawanie debugowania, czyli wypisywania informacji o przebiegu programu i wartościach zmiennych podczas jego wykonywania. To wspaniałe narzędzie, które pozwala zidentyfikować problemy i znacznie przyspieszyć proces rozwiązywania błędów.

## Jak to zrobić

Aby wprowadzić drukowanie informacji debugujących w naszym skrypcie Bash, użyjemy prostego polecenia `echo`. Przykładowo, jeśli chcielibyśmy wyświetlić wartość zmiennej `x`, możemy napisać:

```Bash
echo "Wartość zmiennej x to: $x"
```

Wykorzystując tę prostą komendę, możemy dodawać dowolne informacje o przebiegu naszego programu i wartościach zmiennych w odpowiednich miejscach. W ten sposób uzyskamy szczegółowy obraz działania naszego skryptu.

## Głębsze zanurzenie

Podczas debugowania w Bash istnieje możliwość wprowadzenia specjalnych opcji, które ułatwiają i rozbudowują informacje, które są wyświetlane. Jedną z takich opcji jest `-x`, która umożliwia wyświetlenie całego skryptu i jego poleceń wraz z danymi debugującymi.

Inną przydatną opcją jest `-v`, która wyświetla nie tylko wartości zmiennych, ale również informacje o samych zmiennych, takie jak ich nazwy i wartość domyślna.

Możemy także użyć operatora `set -x` i `set +x` do włączania i wyłączania debugowania dla wybranych części skryptu.

Wszystkie te opcje pozwalają nam na jeszcze dokładniejsze i bardziej spersonalizowane debugowanie naszego programu.

## Zobacz również

- [Bash Cheat Sheet](https://devhints.io/bash)
- [Debugging Bash scripts](https://linux.die.net/man/1/bash)
- [Advanced Bash scripting guide](http://tldp.org/LDP/abs/html/debugging.html)