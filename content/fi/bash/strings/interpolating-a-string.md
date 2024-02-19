---
aliases:
- /fi/bash/interpolating-a-string/
date: 2024-01-20 17:50:14.918678-07:00
description: "Stringien interpolointi tarkoittaa muuttujien ja lausekkeiden sis\xE4\
  llytt\xE4mist\xE4 osaksi merkkijonoja. Koodarit k\xE4ytt\xE4v\xE4t t\xE4t\xE4, koska\
  \ se tekee koodista\u2026"
lastmod: 2024-02-18 23:09:07.787636
model: gpt-4-1106-preview
summary: "Stringien interpolointi tarkoittaa muuttujien ja lausekkeiden sis\xE4llytt\xE4\
  mist\xE4 osaksi merkkijonoja. Koodarit k\xE4ytt\xE4v\xE4t t\xE4t\xE4, koska se tekee\
  \ koodista\u2026"
title: Merkkijonon interpolointi
---

{{< edit_this_page >}}

## What & Why? - Mikä & Miksi?
Stringien interpolointi tarkoittaa muuttujien ja lausekkeiden sisällyttämistä osaksi merkkijonoja. Koodarit käyttävät tätä, koska se tekee koodista selkeämpää ja muokattavuuden joustavammaksi.

## How to: - Kuinka:
```Bash
# Muuttujien käyttö merkkijonon sisällä
kayttaja="Maija"
tervehdys="Hei, $kayttaja!"
echo $tervehdys  # Tulostaa: Hei, Maija!

# Komennon suorittaminen ja tuloksen sijoittaminen merkkijonoon
tiedosto_lista=$(ls)
echo "Tiedostot: $tiedosto_lista"

# Uutta riviä ('\n') käyttävä esimerkki
echo -e "Ensimmäinen rivi\nToinen rivi"
```

## Deep Dive - Syväsukellus:
Alun perin käytettiin kovakoodattuja merkkijonoja, mutta interpolointi otettiin käyttöön, jotta koodi mukautuisi eri tilanteisiin. Alternatiiveja ovat erityisesti kiinteät merkkijonot ja merkkijonojen yhdistäminen. Bashissa interpoloinnin voi toteuttaa käyttämällä kaksoispisteitä ympäröiviä muuttujia tai käyttämällä `command substitution` -ominaisuutta käsittämällä komennon backticks (`) tai $( ) sisään. Implementointi perustuu, että Bash korvaa muuttujan tai lausekkeen arvon oikealla arvolla suorituksenaikana.

## See Also - Katso Myös:
- Bash-harjoituksia: https://www.learnshell.org/
- Bash-skriptauksen opas: https://mywiki.wooledge.org/BashGuide
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
