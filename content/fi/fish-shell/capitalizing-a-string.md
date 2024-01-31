---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
date:                  2024-01-19
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 
Mikä ja miksi?

Tekstijonojen kirjainten muuttaminen suuriksi eli kapitalisointi on usein tarpeen, kun halutaan standardisoida datan ulkoasu, esimerkiksi otsikoissa tai kun muotoillaan käyttäjän nimiä. Ohjelmoijat tekevät tätä esittääkseen tekstin selkeämmin tai täyttääkseen tekniset vaatimukset.

## How to:
Miten:

```Fish Shell
function capitalize
    for word in $argv
        echo -n (string sub -s 1 -- $word | string upper)(string sub -l (math (string length $word) - 1) -- $word)
        echo -n " "
    end
    echo ""
end

# Esimerkin käyttö:
set name "teppo testaaja"
capitalize $name
```

Tulostaa:

```
Teppo Testaaja
```

## Deep Dive
Syväsukellus:

Fish Shellissä ei ole sisäänrakennettua komentoa yksittäisen merkkijonon kapitalisoimiseksi. Olemme luoneet `capitalize` funktion, joka käy läpi sanat ja muuttaa jokaisen sanan ensimmäisen kirjaimen suureksi. Näin voi käsitellä lauseita, ei pelkästään sanoja. Kapitalisointi on käytännöllistä otsikoiden tai nimien käsittelyssä, missä erottuvuus ja muotoilusäännöt ovat tärkeitä.

Vaihtoehtoisesti, jos olisi tarve vain tehdä kaikki kirjaimet suuriksi, voimme käyttää `string upper`-komentoa suoraan. Tarinan toisessa päässä `string lower` pienentää kaikki kirjaimet. Nämä komennot esiteltiin Fish-kalvoilla kauan ennen kuin ohjelmointi oli niin selkeä ja johdonmukainen kuin tänä päivänä. Uudemmissa versioissa käytetään sanojen ensimmäisen kirjaimen käsittelyyn alistring-toimintoja (`string sub`) yhdistettynä merkkijonojen muuntamiseen (`string upper/lower`).

## See Also
Katso myös:

- Fish Shell dokumentaatio: string-komennot: https://fishshell.com/docs/current/cmds/string.html
- Shell-ohjelmoinnin perusteet: https://linuxcommand.org/lc3_learning_the_shell.php
- Suuret ja pienet kirjaimet ohjelmoinnissa: https://en.wikipedia.org/wiki/Letter_case#Programs
