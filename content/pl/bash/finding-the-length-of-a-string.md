---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Bash: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

**## Dlaczego**

Jeśli jesteś początkującym lub nawet doświadczonym programistą, to z pewnością natknąłeś się już na różne zadania związane z manipulacją ciągami tekstowymi. Jednym z takich zadań może być obliczenie długości ciągu znaków. Dlaczego warto poznać ten temat? Ponieważ jest to podstawowa umiejętność, która przyda się w wielu sytuacjach podczas programowania w Bashu.

**## Jak to zrobić**

Aby obliczyć długość ciągu znaków w Bashu, musimy skorzystać z wbudowanej komendy `expr`. Przed jej użyciem musimy jednak pamiętać o kilku ważnych rzeczach. Przede wszystkim, `expr` przyjmuje tylko jeden argument, więc jeśli chcemy obliczyć długość ciągu, musimy go umieścić w cudzysłowie. Ponadto, aby przeczytać wynik z komendy `expr`, musimy przekierować go do zmiennej, w której będziemy go przechowywać.

Przykładowy kod wykorzystujący `expr` do obliczenia długości ciągu `Hello World` wyglądałby następująco:

```Bash
my_string="Hello World"
length=$(expr length "$my_string")

echo $length # wyświetli 11
```
Jeśli chcemy wyświetlić długość ciągu w jednym wierszu wraz z komunikatem, możemy użyć potoku (`|`) i składni `printf`:

```Bash
printf "Długość ciągu 'Hello World' to: $(expr length 'Hello World')\n"
```

**## Deep Dive**

Jak już wspomnieliśmy wcześniej, `expr` przyjmuje tylko jeden argument, dlatego musimy umieścić ciąg znaków w cudzysłowie. W przeciwnym razie, jeśli ciąg zawiera spacje lub inne znaki specjalne, zostaną one traktowane jako oddzielne argumenty, co może skutkować błędnym wynikiem lub nawet błędem.

Ponadto, warto wiedzieć, że `expr` ma także inne funkcje, takie jak obliczanie wartości arytmetycznych lub porównywanie liczb. Możesz przeczytać więcej o tych funkcjach korzystając z komendy `man expr`.

**See Also**

Jeśli chcesz dowiedzieć się więcej o manipulacji ciągami znaków w Bashu, możesz przeczytać te artykuły:

- [Manipulacja ciągami znaków w Bashu](https://linuxhint.com/bash_string_manipulation/)
- [Manipulacja ciągami znaków w Bashu - poradnik dla początkujących](https://blog.balthazar-rouberol.com/manipulating-strings-in-bash)