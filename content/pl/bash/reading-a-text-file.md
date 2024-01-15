---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Bash: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Cześć Czytelniku! Czy kiedykolwiek zastanawiałeś się, jak przeczytać plik tekstowy w Bash? Może próbowałeś wcześniej, ale masz wrażenie, że nie wiesz do końca, jak się za to zabrać? Nie martw się, ten artykuł jest właśnie dla Ciebie! Dowiesz się tutaj w jaki sposób możesz łatwo i szybko odczytać plik tekstowy w języku Bash.

## Jak to zrobić

Aby przeczytać plik tekstowy w Bash, potrzebujemy dwóch komend - 'cat' i 'less'. Pierwsza komenda, 'cat' (od 'concatenate'), wyświetla zawartość pliku na ekranie. Możemy użyć jej w następujący sposób:

```Bash
cat moj_plik.txt
```

Ta komenda wyświetli całą zawartość pliku tekstowego 'moj_plik.txt' na ekranie terminala. Proste, prawda?

Alternatywną komendą jest 'less', która działa podobnie jak 'cat', ale umożliwia przewijanie w górę i w dół tekstu przy użyciu strzałek na klawiaturze. Możemy użyć jej w następujący sposób:

```Bash
less moj_plik.txt
```

Aby przewijać w dół, należy nacisnąć strzałkę w dół, a aby wyjść z trybu przewijania należy wpisać 'q' na klawiaturze. Proste, prawda?

## Deep Dive

Aby lepiej zrozumieć jak działają te komendy, musimy cofnąć się do podstaw działania systemów operacyjnych. Linux i MacOS (oraz inne podobne systemy UNIX) traktują wszystko jako pliki. Oznacza to, że zarówno pliki tekstowe, jak i foldery są po prostu plikami. W przypadku plików tekstowych, komenda 'cat' odczytuje je w całości i wyświetla ich zawartość, a 'less' umożliwia nam przewijanie, ponieważ jest to bardziej zaawansowana komenda.

## Zobacz też

- Dokumentacja systemu Linux: [www.linux.die.net/man/1/cat](www.linux.die.net/man/1/cat)
- Dokumentacja systemu MacOS: [developer.apple.com/library/archive/documentation/](developer.apple.com/library/archive/documentation/)/OpenSource/Reference/ManPages/man1/cat.1.html
- Strona domowa języka Bash: [www.gnu.org/software/bash/](www.gnu.org/software/bash/)