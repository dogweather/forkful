---
title:                "Väliaikaistiedoston luominen"
aliases:
- fi/fish-shell/creating-a-temporary-file.md
date:                  2024-01-20T17:40:08.928034-07:00
model:                 gpt-4-1106-preview
simple_title:         "Väliaikaistiedoston luominen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Temp-tiedoston luonti on väliaikaisten, usein kertakäyttöisten tiedostojen tekemistä. Ohjelmoijat käyttävät niitä esimerkiksi datan väliaikaissäilytykseen tai testitilanteissa.

## How to: (Kuinka tehdä:)
```Fish Shell
# Luo väliaikainen tiedosto
set tmp_file (mktemp)

# Käytä tiedostoa
echo 'Tämä on testi' > $tmp_file

# Tulostetaan väliaikaisen tiedoston sisältö
cat $tmp_file

# Poista väliaikainen tiedosto lopuksi
rm $tmp_file
```

Tulostus näyttäisi tältä:
```
Tämä on testi
```

## Deep Dive (Syväsukellus)
Fish Shell käyttää `mktemp`-ohjelmaa väliaikaisten tiedostojen luontiin. Se on UNIX-standardi ja löytyy useimmista käyttöjärjestelmistä. Vaihtoehtoisesti voitaisiin käyttää `tmpfile()`-funktiota C-ohjelmoinnissa, mutta Fishissä `mktemp` on kätevä ja nopea. Tärkeää on muistaa poistaa temp-tiedostot, etteivät ne täytä levytilaa.

## See Also (Katso Myös)
- Fish Shell Documentation: https://fishshell.com/docs/current/index.html
- UNIX `mktemp` Manual Page: https://man7.org/linux/man-pages/man1/mktemp.1.html
- Bash vs. Fish Shell Differences: https://www.2daygeek.com/bash-vs-fish-shell-differences/
