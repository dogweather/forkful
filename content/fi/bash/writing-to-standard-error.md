---
title:                "Kirjoittaminen vakiovirheeseen"
date:                  2024-01-19
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä ja miksi?
Kirjoittaminen standard erroriin (stderr) ohjaa virheilmoitukset tai varoitukset erilliseen virtaan. Ohjelmoijat käyttävät tätä erotellakseen normaalin tulosteen (stdout) ja virhetilanteiden raportoinnin, mikä helpottaa debuggausta ja lokien käsittelyä.

## How to: - Näin teet:
```Bash
# Kirjoittaminen stderr-virtaan
echo "Tämä on virheilmoitus" >&2

# Esimerkki ohjelmasta, joka tulostaa viestin stderriin ja stdoutiin
#!/bin/bash
echo "Tämä menee tavalliseen tulosteeseen"
echo "Tämä on virheilmoitus" >&2
```
Esimerkkitulostus:
```
Tämä menee tavalliseen tulosteeseen
Tämä on virheilmoitus
```

## Deep Dive - Syväsukellus:
Stderr luotiin Unix-järjestelmissä erottamaan virhetulostus ohjelman muusta tulosteesta. Vaihtoehtona stderrille voisi käyttää lokitiedostoja, mutta stderr on standardisoitu ja välitön tapa raportoida ongelmat. Stderriin kirjoittaminen käyttää tiedostopiirrettä 2 (`2>`), kun taas stdout käyttää tiedostopiirrettä 1.

## See Also - Katso myös:
- Bashin virallinen dokumentaatio: https://www.gnu.org/software/bash/manual/bash.html
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
- Unix & Linux Stack Exchange: https://unix.stackexchange.com/
