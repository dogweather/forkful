---
date: 2024-01-20 17:34:16.195536-07:00
description: "How to: (Jak to zrobi\u0107:) ."
lastmod: '2024-03-13T22:44:35.574594-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 3
---

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
