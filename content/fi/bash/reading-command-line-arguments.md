---
title:                "Komennoriviparametrien lukeminen"
date:                  2024-01-20T17:55:32.343899-07:00
model:                 gpt-4-1106-preview
simple_title:         "Komennoriviparametrien lukeminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Komennon rivin argumentit ovat syötteitä, joita ohjelmat saavat käynnistyessään komentoriviltä. Ne mahdollistavat ohjelmalle eri tilanteisiin mukautumisen ilman koodimuutoksia.

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
