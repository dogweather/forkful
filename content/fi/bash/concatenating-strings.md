---
title:                "Merkkijonojen yhdistäminen"
aliases:
- fi/bash/concatenating-strings.md
date:                  2024-01-20T17:34:01.597668-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonojen yhdistäminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä ja miksi?
Yksinkertaistettuna, merkkijonojen yhdistäminen on tapa liittää tekstejä yhteen. Ohjelmoijat tekevät tätä, koska tarvitsevat usein luoda dynaamisia viestejä, polkuja tai komentoja.

## How to - Kuinka tehdään:
```Bash
# Merkkijonojen yhdistäminen liittämisellä
tervehdys="Hei"
nimi="Maailma"
viesti="$tervehdys, $nimi!"
echo $viesti  # Tulostaa: Hei, Maailma!

# Yhdessä rivissä ilman välispaita
echo "Hyvää"${tervehdys}"a, "$nimi"!"  # Tulostaa: HyvääHeiä, Maailma!

# Toisen merkkijonon lisääminen käyttäen +=
loppu="n loppu."
viesti+=" Ja tässä"$loppu
echo $viesti  # Tulostaa: Hei, Maailma! Ja tässä on loppu.
```

## Deep Dive - Syväsukellus:
Merkkijonojen yhdistäminen on ollut osa ohjelmointia iät ja ajat. Sellaiset kielet kuin C käyttivät toimintoja kuten `strcat`, mutta Bash tekee tämän suoraan muuttujien kanssa.

Vaihtoehdot yhdistämiselle ovat tullut ja menneet, mutta liittäminen muuttujien avulla pysyy selkeänä ja nopeana.

Erityisesti, Bashissa ei ole perinteistä merkkijono "objektia" eikä tarvitse käsitellä muistinhallintaa samalla tavalla kuin joissain muissa kielissä.

## See Also - Katso myös:
- Bash-hahmonmuodostus ja lainausmerkkien käyttö: https://www.gnu.org/software/bash/manual/bash.html#Quoting
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
- Bash-haarojen käsittely ja merkkijonokäsittelyn laajentaminen: https://mywiki.wooledge.org/BashFAQ/100
