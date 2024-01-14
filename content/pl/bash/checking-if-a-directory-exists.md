---
title:                "Bash: Sprawdzanie czy istnieje katalog"
simple_title:         "Sprawdzanie czy istnieje katalog"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu zdarza się, że musimy sprawdzić, czy dany katalog istnieje. Może to być konieczne, gdy chcemy upewnić się, że nasz program będzie działał poprawnie lub gdy potrzebujemy określonej ścieżki do pliku. W tym artykule przedstawimy sposób, w jaki można to zrobić w języku Bash.

## Jak to zrobić

Do sprawdzenia istnienia katalogu w Bash możemy wykorzystać komendę `test`. Przyjmie ona dwa argumenty: opcję `-d` oznaczającą, że chcemy sprawdzić czy dany element jest katalogiem oraz ścieżkę do danego elementu. Przykładowy kod wyglądałby następująco:

```Bash
if test -d /ścieżka/do/katalogu
then
  echo "Katalog istnieje."
else
  echo "Katalog nie istnieje."
fi
```

W powyższym przykładzie wykorzystujemy polecenie `echo`, które służy do wypisywania tekstu na ekranie. Możemy także wykorzystać instrukcję `if`, która sprawdzi, czy komenda `test` zwróciła wartość prawdy, czyli czy katalog istnieje. Wówczas wypiszemy odpowiedni komunikat.

## Zagłębienie się

Jeśli chcemy zagłębić się bardziej w temat sprawdzania istnienia katalogu w Bash, warto wspomnieć o innych możliwych opcjach dla komendy `test`. Oprócz `-d` możemy także użyć między innymi `-e`, sprawdzającej istnienie elementu niezależnie od jego typu, czy `-s`, która sprawdzi, czy element jest niepusty. Komenda `test` może też przyjąć więcej niż jeden argument, co pozwala na sprawdzenie istnienia kilku katalogów naraz.

## Zobacz także

- [Dokumentacja dla komendy test w Bash (ang.)](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html#Bash-Conditional-Expressions)
- [Poradnik GeekStuff o sprawdzaniu istnienia plików i katalogów w Bash (ang.)](https://www.geeksforgeeks.org/bash-test-whether-directory-exists/)
- [Komenda test na stronie Linux.com (ang.)](https://www.linux.com/training-tutorials/painless-bash-testing-checking-file-and-directory-statistics-with-test/)