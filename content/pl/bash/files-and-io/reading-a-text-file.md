---
date: 2024-01-20 17:53:47.529705-07:00
description: "Przeczytanie pliku tekstowego w Bashu to po prostu wy\u015Bwietlenie\
  \ jego zawarto\u015Bci w terminalu. Programi\u015Bci robi\u0105 to do analizy danych,\
  \ debugowania i\u2026"
lastmod: '2024-03-13T22:44:35.602781-06:00'
model: gpt-4-1106-preview
summary: "Przeczytanie pliku tekstowego w Bashu to po prostu wy\u015Bwietlenie jego\
  \ zawarto\u015Bci w terminalu."
title: Odczytywanie pliku tekstowego
weight: 22
---

## Jak to zrobić:
```Bash
# Wyświetlenie całego pliku używając komendy 'cat':
cat plik.txt

# Wyświetlenie pliku strona po stronie z 'less':
less plik.txt

# Użycie pętli 'while' do przeczytania pliku linia po linii:
while IFS= read -r line; do
    echo "$line"
done < plik.txt

# Wypisywanie numerów linii wraz z zawartością:
cat -n plik.txt
```

Output przykładowy dla `cat plik.txt`:
```
To jest pierwsza linia pliku.
To jest druga linia pliku.
```

## Głębsze zanurzenie
Początki poleceń do czytania plików sięgają wczesnych lat tworzenia systemów uniksowych. 'Cat', 'less' i 'tail' to klasyka, a końcówka TXT dla plików tekstowych pochodzi z czasów ograniczeń systemów DOS.

Alternatywami dla 'cat' są 'more' i 'less', które mogą być przydatne przy większych plikach. 'Tak' pozwala śledzić zmiany w pliku w czasie rzeczywistym.

Detaily implementacyjne:
- `IFS=` zapobiega interpretowaniu białych znaków jako separatorów.
- `read -r` uniemożliwia interpretację backslashów.
- Użycie `<` przekierowuje zawartość pliku do pętli.

## Zobacz również
- [The GNU Operating System and the Free Software Movement](https://www.gnu.org/)
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/)
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/)
