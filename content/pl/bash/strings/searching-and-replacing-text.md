---
date: 2024-01-20 17:57:12.712992-07:00
description: "Jak to zrobi\u0107: Polecenie `sed` (stream editor) to klasyk w UNIX-owych\
  \ systemach, u\u017Cywany od lat '70. Alternatywami dla `sed` mog\u0105 by\u0107\
  \ `awk`, `perl`, czy\u2026"
lastmod: '2024-04-05T22:50:49.891492-06:00'
model: gpt-4-1106-preview
summary: "Polecenie `sed` (stream editor) to klasyk w UNIX-owych systemach, u\u017C\
  ywany od lat '70."
title: Wyszukiwanie i zamiana tekstu
weight: 10
---

## Jak to zrobić:
```Bash
# Wyszukaj i zamień tekst w pliku za pomocą sed
sed -i 's/szukany_tekst/nowy_tekst/g' plik.txt

# Przykład zamiany 'kot' na 'pies' w pliku 'zwierzeta.txt'
sed -i 's/kot/pies/g' zwierzeta.txt

# Wypisz wynik zamiany na ekranie
sed 's/kot/pies/g' zwierzeta.txt
```
Output:
```Bash
Jeden pies, dwa psy, trzy psy.
```

## A na głębszą wodę:
Polecenie `sed` (stream editor) to klasyk w UNIX-owych systemach, używany od lat '70. Alternatywami dla `sed` mogą być `awk`, `perl`, czy nowoczesne skrypty w Pythonie, których wybór zależy od złożoności zadania i preferencji programisty. Ważne jest, że `sed` działa na strumieniach, przez co jest szybki i sprawnie przetwarza nawet duże pliki. 

Flaga `-i` w `sed` oznacza zapisanie zmian bezpośrednio w pliku. `g` na końcu wzorca zamiany mówi o globalnej zamianie – bez niej `sed` zmieni tylko pierwsze wystąpienie w linii.

## Zobacz także:
- [GNU sed manual](https://www.gnu.org/software/sed/manual/sed.html) – szczegółowa dokumentacja `sed`.
- [Regular Expressions](https://www.regular-expressions.info/) – poradnik i tutorial dotyczący wyrażeń regularnych.
- [Bash Scripting Tutorial](https://ryanstutorials.net/bash-scripting-tutorial/) – ogólny kurs skryptowania w Bashu.
