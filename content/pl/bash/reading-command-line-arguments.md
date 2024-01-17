---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "Bash: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Czy kiedykolwiek zastanawiałeś się, jak programiści mogą wpłynąć na działanie swoich programów z poziomu wiersza poleceń? Rozwiązaniem jest czytanie argumentów przekazanych do linii poleceń. W ten sposób programista może dostosować działanie programu w zależności od użytkownika, bez konieczności zmiany samego kodu.

## Jak to zrobić?

Aby czytać argumenty z wiersza poleceń, wystarczy użyć specjalnej składni w języku Bash. Przykładowo, programista może odczytać argumenty zapisane po dyrektywie ```$1```, ```$2```, itd. Poniżej przedstawiony jest prosty skrypt, który wypisze pierwszy i drugi argument przekazany do programu.

```Bash
#!/bin/bash
echo "Pierwszy argument: $1"
echo "Drugi argument: $2"
```

Po uruchomieniu skryptu z argumentami np. ```./skrypt.sh hello world```, program wyświetli następujący wynik:

```
Pierwszy argument: hello
Drugi argument: world
```

## Głębsza analiza

Czytanie argumentów z wiersza poleceń jest powszechnie używane od lat w programowaniu. Podczas gdy można to zrobić z użyciem innych języków programowania, język Bash jest idealny do tego rodzaju zadań, ze względu na wygodną składnię i dostępność na większości systemów operacyjnych. Istnieją również inne metody odczytywania argumentów, takie jak wtyczki dla IDE czy specjalne biblioteki, ale odczytywanie z wiersza poleceń jest często szybsze i wygodniejsze.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o odczytywaniu argumentów z wiersza poleceń w języku Bash, znajdziesz wiele przydatnych informacji w dokumentacji oficjalnego składni systemu Bash, dostępnej na stronie [GNU](https://www.gnu.org/software/bash/manual/html_node/index.html). Możesz również przeczytać artykuł na temat przetwarzania argumentów z wiersza poleceń w języku Bash na [Medium](https://medium.com/@jasonrigden/a-guide-to-constructing-a-bash-scripting-arguments-54dd5c2f6c2f). Śmiało, eksperymentuj i udoskonalaj swoje umiejętności w Bash!