---
title:    "Bash: Sprawdzanie, czy istnieje katalog"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że przy pisaniu skryptów w Bash potrzebujemy sprawdzić, czy dany katalog istnieje czy też nie. Może to być konieczne do przeprowadzenia odpowiednich operacji lub wyboru odpowiedniego ścieżki. Dlatego warto poznać sposób na sprawdzenie istnienia katalogu w języku Bash.

## Jak to zrobić

Sprawdzenie, czy dany katalog istnieje, jest bardzo proste w Bash. Wystarczy użyć polecenia `test` lub skrótu `[` i przekazać jako argument `d`, co oznacza katalog (directory), oraz ścieżkę do katalogu, który chcemy sprawdzić. Na przykład:

```bash
if [ -d ~/Documents ]; then
  echo "Katalog ~/Documents istnieje."
else
  echo "Katalog ~/Documents nie istnieje."
fi
```

Powyższy przykład sprawdza, czy katalog "Documents" znajdujący się w katalogu domowym użytkownika istnieje. Jeśli istnieje, wyświetla odpowiedni komunikat, w przeciwnym razie wyświetla inny. Możemy również użyć polecenia `mkdir` w celu utworzenia katalogu, jeśli ten nie istnieje, na przykład:

```bash
if [ ! -d ~/Documents/NewFolder ]; then
  mkdir ~/Documents/NewFolder
else
  echo "Katalog ~/Documents/NewFolder już istnieje."
fi
```

W powyższym przykładzie, jeśli katalog "NewFolder" nie istnieje, zostanie on utworzony. W przeciwnym razie wyświetli się odpowiedni komunikat.

## Wnikliwa analiza

W języku Bash istnieje wiele innych warunkowych poleceń, które można użyć do sprawdzania istnienia katalogu, na przykład `if [ -e ~/Documents ]`, które sprawdza, czy dany plik lub katalog istnieje, lub `if [ -r ~/Documents ]`, które sprawdza, czy dany plik lub katalog jest dostępny do odczytu. Dzięki temu, że sama składnia języka Bash jest dość prosta i intuicyjna, możemy w błyskawiczny sposób sprawdzić wiele różnych warunków i podjąć odpowiednie działania.

Ważne jest również, aby pamiętać, że skrypt w Bash wykonywany jest w określonym katalogu, więc ścieżki do katalogów mogą być względne lub bezwzględne, co może mieć wpływ na wynik wykonywanych operacji. Dlatego zawsze warto sprawdzać i upewniać się, w jakim katalogu jesteśmy w danym momencie.

## Zobacz także

- [Programowanie w Bash - samouczek od podstaw](https://www.rytmy.pl/bash/wprowadzenie-do-programowania-w-bash/)
- [Oficjalna dokumentacja języka Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Kurs Bash w Codecademy](https://www.codecademy.com/learn/learn-the-command-line)