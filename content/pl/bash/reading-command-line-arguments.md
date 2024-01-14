---
title:    "Bash: Odczytywanie argumentów wiersza poleceń"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista, niezależnie od poziomu zaawansowania, musi nauczyć się obsługiwać argumenty wiersza poleceń. Jest to niezbędne w tworzeniu skryptów lub aplikacji, które wymagają interakcji z użytkownikiem. Pozwala to na dynamiczne wprowadzanie danych i dostosowywanie działania programu w zależności od potrzeb. W tym artykule dowiesz się, jak czytać argumenty wiersza poleceń w języku Bash.

## Jak to zrobić

Aby odczytać argumenty wiersza poleceń, należy skorzystać z wbudowanych zmiennych środowiskowych, takich jak $1, $2, $3 itd. Wartość tych zmiennych zależy od przekazanych argumentów. Można je również przypisać do innych zmiennych, co ułatwia pracę z nimi.

```Bash
# Przykład: Skrypt, który wyświetla przekazane argumenty

#!/bin/bash
echo "Otrzymano następujące argumenty:"
echo $1
echo $2
```

#### Przykładowe wywołanie skryptu z argumentami:
```Bash
./skrypt.sh argument1 argument2
```

#### Przykładowy wynik:
```
Otrzymano następujące argumenty:
argument1
argument2
```

## Wnikliwa analiza

Przekazane argumenty są przechowywane w tablicy, której nazwa to "$@", a jej elementy są indeksowane od 0. Warto zauważyć, że argumenty przekazywane w cudzysłowach będą traktowane jako pojedynczy argument, a spację można oddzielić użyciem symbolu "\".

Możliwe jest również przekazanie argumentów bezpośrednio do pętli "for", co ułatwia iterację po wszystkich argumentach.

```Bash
for arg in "$@"
do
    echo "Przekazano argument: $arg"
done
```

#### Przykładowy wynik:
```
Przekazano argument: argument1
Przekazano argument: argument2
```

## Zobacz także

* [Oficjalna dokumentacja Bash](https://www.gnu.org/software/bash/manual/bash.html)
* [Poradnik Bash na Codecademy](https://www.codecademy.com/learn/learn-the-command-line/modules/bash-scripting)
* [Przykłady zastosowań argumentów wiersza poleceń](https://www.howtogeek.com/435903/how-to-use-command-line-arguments-in-bash/)

Dzięki temu artykułowi powinieneś mieć już podstawową wiedzę na temat czytania argumentów wiersza poleceń w języku Bash. Będąc programistą, warto umieć wykorzystywać przydatne narzędzia, takie jak narzędzia linii poleceń, aby ułatwić sobie pracę. W razie potrzeby, zawsze można poszerzyć swoją wiedzę, korzystając z przytoczonych źródeł.