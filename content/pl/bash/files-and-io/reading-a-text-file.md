---
date: 2024-01-20 17:53:47.529705-07:00
description: "Jak to zrobi\u0107: Output przyk\u0142adowy dla `cat plik.txt`."
lastmod: '2024-04-05T21:53:37.028640-06:00'
model: gpt-4-1106-preview
summary: "Output przyk\u0142adowy dla `cat plik.txt`."
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
