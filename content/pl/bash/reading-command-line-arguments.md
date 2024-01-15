---
title:                "Odczytywanie argumentów z wiersza poleceń"
html_title:           "Bash: Odczytywanie argumentów z wiersza poleceń"
simple_title:         "Odczytywanie argumentów z wiersza poleceń"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub osobą, która często korzysta z terminala, na pewno słyszałeś o parametrach wiersza poleceń. Przeczytaj ten artykuł, aby dowiedzieć się, dlaczego warto znać i umieć czytać argumenty wiersza poleceń w Bash.

## Jak

Zacznijmy od podstaw. Najprostszym sposobem na przekazanie argumentów wiersza poleceń jest użycie zmiennej specjalnej `$1` dla pierwszego argumentu, `$2` dla drugiego argumentu itd.

```Bash
#!/bin/bash

echo "Pierwszy argument: $1"
echo "Drugi argument: $2"
```

Zapisz ten kod w pliku "arguments.sh" i uruchom go z dwoma argumentami: 

`$ bash arguments.sh hello world`

Powinieneś zobaczyć następujący wynik:

```
Pierwszy argument: hello
Drugi argument: world
```

Możesz również użyć składni `"$@"` dla wszystkich argumentów. W ten sposób możesz przekazać wiele argumentów do skryptu bez konieczności odwoływania się do każdego z osobna.

```Bash
#!/bin/bash

echo "Wszystkie argumenty: $@"
```

Teraz przetestuj ten kod z trzema argumentami: 

`$ bash arguments.sh one two three`

Wynik powinien być następujący:

```
Wszystkie argumenty: one two three
```

## Deep Dive

Istnieje wiele innych sposobów na czytanie argumentów wiersza poleceń w Bash, takich jak polecenie `getopts` do parsowania opcji i argumentów lub używanie pętli `for` do przetwarzania wszystkich argumentów. Możesz również wykorzystać zmienne globalne, takie jak `OPTARG` i `OPTIND`. 

Warto poznać również pewne przydatne zmienne, takie jak `#` dla liczby argumentów, `*` dla wszystkich argumentów połączonych w jedną ciągłą linię, lub `?` dla ostatniego błędnego argumentu. 

Pamiętaj, że Bash obsługuje również opcjonalne argumenty flagowe z użyciem znaku "-" przed nazwą flagi. Możesz również przypisać wartości domyślne dla argumentów, gdy nie są przekazywane do skryptu.

## Zobacz również

Jeśli chcesz poznać więcej o tym, jak czytać argumenty wiersza poleceń w Bash, polecam przeczytać dokumentację na ten temat lub zapoznać się z innymi przydatnymi przykładami.

- [Oficjalna dokumentacja Bash](https://www.gnu.org/software/bash/)
- [Przykłady użycia argumentów wiersza poleceń w Bash](https://linuxhint.com/args-bash/)
- [Przewodnik do czytania argumentów wiersza poleceń w Bash](https://www.lifewire.com/pass-arguments-in-shell-script-2200571)