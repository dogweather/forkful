---
title:                "Bash: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Witaj w moim blogu poświęconym programowaniu w Bash. Dziś skupimy się na temacie wydobywania podciągów z tekstu, co może wydawać się być zadaniem trywialnym, ale może być bardzo przydatne w wielu sytuacjach. Dlatego zachęcam do lektury tego wpisu, aby nauczyć się jak wykorzystać tę technikę w swoim kodzie.

## Jak to zrobić

Wydobywanie podciągów z tekstu w Bash może być osiągnięte za pomocą kilku metod, w zależności od potrzeb i preferencji programisty. Jedną z najprostszych metod jest użycie wbudowanego operatora `:` oraz ukośników (`/`) do określenia początku i końca naszego podciągu.

```Bash
# Przykładowy tekst
string="Dzień dobry, witaj w moim blogu!"

# Wydobywanie podciągu od indeksu 6 do końca
echo ${string:6}

# Wynik: dobry, witaj w moim blogu!
```

Możemy również określić początkowy i końcowy indeks naszego podciągu, aby wydobyć tylko pewną część tekstu.

```Bash
# Wydobywanie podciągu od indeksu 6 do 15
echo ${string:6:15}

# Wynik: dobry, witaj w
```

Inną metodą jest użycie wbudowanej funkcji `cut` w Bash. Wymaga ona podania separatora oraz numerów pola, z którego chcemy wydobyć podciąg.

```Bash
# Użycie funkcji cut z separatorem przecinka
echo $string | cut -d"," -f1

# Wynik: Dzień dobry
```

Możemy też skorzystać z funkcji `sed`, która jest potężnym narzędziem do manipulacji tekstem. Tutaj przykładowo wydobywamy podciąg znajdujący się między dwoma słowami.

```Bash
# Znajdujemy słowa "miło" i "blog"
# i wydobywamy wszystko pomiędzy nimi
echo $string | sed 's/.*miło\(.*\)blog.*/\1/'

# Wynik: w moim
```

## Deep Dive

Wydobywanie podciągów z tekstu jest przydatną, ale prostą funkcjonalnością w Bash. Jednak warto pamiętać, że operacje na tekstach w Bash nie są tak wydajne jak w językach wysokiego poziomu, dlatego lepiej unikać wykorzystywania ich w przypadku dużych i złożonych tekstów.

Bardziej rozbudowane wyrażenia regularne mogą również zostać wykorzystane do bardziej precyzyjnego wydobywania podciągów. Warto również pamiętać o możliwości użycia pętli i warunków, aby dynamicznie wydobywać podciągi w zależności od zmieniających się danych.

## Zobacz również

Chcesz dowiedzieć o wydobywania podciągów w Bash więcej? Sprawdź poniższe linki, aby zgłębić swoją wiedzę:

- [Dokumentacja Bash](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Tutorial na FreeCodeCamp](https://www.freecodecamp.org/news/bash-string-manipulation/)

Dziękujemy za przeczytanie naszego wpisu, mam nadzieję że dowiedziałeś się czegoś ciekawego i użytecznego. Do zobaczenia w kolejnych artykułach!