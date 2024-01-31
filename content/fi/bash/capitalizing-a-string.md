---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
date:                  2024-01-19
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Merkkijonojen suurentaminen tarkoittaa joko jokaisen sanan ensimmäisen kirjaimen tai koko merkkijonon muuttamista isoksi kirjaimeksi. Tämä auttaa tekstin erottumaan tai noudattamaan formaattistandardeja, kuten otsikoissa tai tunnisteissa.

## How to:
```Bash
# Merkkijonon muuttaminen kokonaan isoksi kirjaimeksi
echo "moikka maailma" | tr '[:lower:]' '[:upper:]'
# Tulostuu: MOIKKA MAAILMA

# Vain sanan ensimmäisen kirjaimen suurentaminen
echo "moikka maailma" | awk '{for(i=1;i<=NF;i++) $i=toupper(substr($i,1,1)) substr($i,2)} 1'
# Tulostuu: Moikka Maailma
```

## Deep Dive:
Komentotulkissa merkkijonojen suurentaminen ei ole uusin konsepti. Se on peräisin ajoilta, kun ohjelmat ja järjestelmät alkoivat käsitellä tekstiä ja kaipasivat tapoja muotoilla sitä. `tr` ja `awk` ovat klassisia työkaluja, jotka ovat olleet käytössä jo vuosikymmeniä.

`tr` on yksinkertainen työkalu merkkien muuntamiseen; se ei ymmärrä sanoja, rivejä eikä lausekkeita, vain merkkejä. `awk` on tehokas tekstinkäsittelykieli, joka kykenee suorittamaan monimutkaisempia manipulaatioita, kuten sanakohtaisen pääkirjainmuunnoksen.

Bash-funktioita tai moderneja työkaluja kuten `sed` voi myös käyttää, mutta tässä on tärkeää tuntea työkalun syntaksi ja kyvyt. Pelkistetysti, Bash ei sisällä sisäänrakennettua komentoa pääkirjainmuunnokselle, mutta sen sijasta se tukee useita yleisiä työkaluja, joilla sama lopputulos saavutetaan.

## See Also:
- `man tr`: Kertoo lisää tr-komennon käyttämisestä ja vaihtoehdoista.
- `man awk`: Antaa tietoa awk-komennosta ja sen syntaksista.
- Bash String Manipulation -opas: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
