---
title:                "Łączenie łańcuchów znaków"
date:                  2024-01-20T17:34:16.195536-07:00
model:                 gpt-4-1106-preview
simple_title:         "Łączenie łańcuchów znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Konkatenacja to fancy słowo na łączenie stringów. Robimy to, żeby zbudować większe ciągi znaków, zarządzać tekstami, ścieżkami plików czy tworzyć komunikaty.

## How to: (Jak to zrobić:)
```Bash
# Połącz dwie zmienne
first="Witaj, "
second="Świecie!"
greeting="${first}${second}"
echo $greeting
```
Output:
```
Witaj, Świecie!
```

```Bash
# Dodaj string bezpośrednio do zmiennej
prefix="Ile ważysz, "
suffix=" kilogramów?"
weight=75
echo "${prefix}${weight}${suffix}"
```
Output:
```
Ile ważysz, 75 kilogramów?
```

```Bash
# Używając polecenia printf
printf -v full_greeting "%s%s" "$first" "$second"
echo $full_greeting
```
Output:
```
Witaj, Świecie!
```


## Deep Dive (Wnikliwa Analiza)
W latach 70., kiedy Unix zyskiwał na popularności, składnia stringów była podstawą. Bash, jako potomek shella z Unix, przejął te konwencje.

Alternatywą dla konkatenacji w Bash są inne języki skryptowe jak Python czy Perl, które mają bardziej zaawansowane operacje na stringach.

Bash traktuje zmienne jako stringi domyślnie, więc nie musisz deklarować typów. Dodatkowo, konkatenacja stringów w Bash jest bezpośrednia - wystarczy umieścić zmienne obok siebie.

## See Also (Zobacz także)
- [Bash String Operations](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/)

Pamiętaj, że Bash jest potężnym narzędziem - z małą ilością kodu możesz zdziałać wiele! Ale również warto wiedzieć, kiedy do zadania lepiej użyć innego języka.
