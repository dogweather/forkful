---
date: 2024-01-20 17:55:32.343899-07:00
description: "How to: K\xE4ytt\xF6j\xE4rjestelm\xE4t ovat hyv\xE4ksyneet komentorivin\
  \ argumentit jo vuosikymmenten ajan ohjelmien tuunaamiseen. Komentotulkin sis\xE4\
  iset muuttujat `$0`,\u2026"
lastmod: '2024-04-05T22:51:10.906881-06:00'
model: gpt-4-1106-preview
summary: "K\xE4ytt\xF6j\xE4rjestelm\xE4t ovat hyv\xE4ksyneet komentorivin argumentit\
  \ jo vuosikymmenten ajan ohjelmien tuunaamiseen."
title: Komennoriviparametrien lukeminen
weight: 23
---

## How to:
```Bash
#!/bin/bash
# Tämä skripti tulostaa kaikki annetut komentorivin argumentit.

echo "Skriptiä kutsuttiin: $0"
echo "Annettujen argumenttien määrä: $#"

echo "Annetut argumentit:"

for arg in "$@"
do
    echo "$arg"
done
```

Käytä skriptiä näin:
```Bash
$ bash skripti.sh arg1 arg2 arg3
Skriptiä kutsuttiin: skripti.sh
Annettujen argumenttien määrä: 3
Annetut argumentit:
arg1
arg2
arg3
```

## Deep Dive
Käyttöjärjestelmät ovat hyväksyneet komentorivin argumentit jo vuosikymmenten ajan ohjelmien tuunaamiseen. Komentotulkin sisäiset muuttujat `$0`, `$1`, `$2`, ..., `$9` vastaavat argumentteja. `$0` on skriptin nimi, `$1` ensimmäinen argumentti, ja niin edelleen. `$#` kertoo argumenttien määrän ja `$@` tai `$*` listaa kaikki argumentit. Bash ei tue yli yhdeksän positionaalista parametria suoraan, mutta `{10}`, `{11}`, jne. avulla voit käsitellä niitä.

`getopts` ja `getopt` ovat komentoja, jotka auttavat monimutkaisempien komentorivin argumenttien hallinnassa, tarjoten vaihtoehtoja ja kytkimiä.

## See Also
Bash-hakkeroinnin syventäminen:
- [GNU Bash documentation](https://www.gnu.org/software/bash/manual/)
- [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/)
- `man bash` – Linux manuaalisivu Bashille, `man getopts` – opas getopts-komennolle.
