---
title:                "Wyszukiwanie i zamiana tekstu."
html_title:           "Bash: Wyszukiwanie i zamiana tekstu."
simple_title:         "Wyszukiwanie i zamiana tekstu."
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

W programowaniu często zachodzi potrzeba zmiany lub zamiany pewnych fragmentów tekstu. Proces ten nazywa się wyszukiwaniem i zastępowaniem tekstu. Programiści często wykonują tę operację w celu usprawnienia swojej pracy lub naprawienia błędów.

## Jak to zrobić?

W Bashu istnieją różne sposoby na wyszukiwanie i zastępowanie tekstu. Jedną z najpopularniejszych komend jest `sed`, której składnia wygląda następująco:

```
sed 's/stary_tekst/nowy_tekst/g' plik.txt
```

Powyższy przykład zmieni wszystkie wystąpienia `stary_tekst` na `nowy_tekst` w pliku `plik.txt` i wypisze wynik na ekranie. Można również zastosować opcję `-i` aby zmienić bezpośrednio treść w pliku.

Inną przydatną komendą jest `awk`, która pozwala na wykonywanie operacji na plikach tekstowych w sposób bardziej zaawansowany. Przykładowe użycie wygląda następująco:

```
awk '{gsub(/stary_tekst/,"nowy_tekst")}1' plik.txt
```

Powyższy przykład zastąpi wszystkie wystąpienia `stary_tekst` na `nowy_tekst` w pliku `plik.txt` i wyświetli wynik na ekranie.

## Głębsze zagadnienia

W wyszukiwaniu i zastępowaniu tekstu istnieje wiele różnych narzędzi i sposobów ich zastosowania. Popularnymi alternatywami dla `sed` i `awk` są między innymi `perl` czy `grep` .

Warto również zauważyć, że operacje wyszukiwania i zastępowania tekstu nie są specyficzne tylko dla Bash'a. Inne języki programowania również oferują podobne mechanizmy, na przykład Python z modułem `re` lub JavaScript z metodą `replace`.

## Zobacz też

- Dokumentacja `sed`: https://www.gnu.org/software/sed/manual/sed.html
- Komenda `grep`: https://www.gnu.org/software/grep/manual/grep.html
- Składnia `perl` do wyszukiwania i zastępowania tekstu: https://perldoc.perl.org/5.30.0/perlre.html
- Moduł `re` w Pythonie: https://docs.python.org/3/library/re.html