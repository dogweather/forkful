---
title:                "Tekstin interpolointi"
html_title:           "Fish Shell: Tekstin interpolointi"
simple_title:         "Tekstin interpolointi"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

Mitä on merkkijonon interpolointi ja miksi ohjelmoijat tekevät sitä?

Merkkijonon interpolointi tarkoittaa merkkijonon sisällä olevien muuttujien arvojen asettamista merkkijonoon. Tämä mahdollistaa muuttujien käytön tekstin joukossa. Ohjelmoijat tekevät tätä helpottaakseen koodin lukemista ja kirjoittamista sekä välttääkseen toistoa.

Miten:
Fish Shellin avulla voit helposti interpoloida merkkijonoja. Alla on esimerkkejä siitä, miten tämä tehdään:

```
set nimi "Maija"
echo "Hei, $nimi!" #Tulostaa "Hei, Maija!"
```

```
set luku 5
echo "Luku on $luku." #Tulostaa "Luku on 5."
```

Syvempää tietoa:
Interpolointi on ollut käytössä jo pitkään eri ohjelmointikielillä, kuten Perlissä ja Bashissa. Fish Shell eroaa näistä käärittymällä koodinpätkät sulkeiden sijasta aaltosulkeihin. Myös muuttujien syöttö Fish Shelliin on helpompaa ja intuitiivisempaa kuin perinteisessä Bashissa.

Vaihtoehtoja interpolointiin ovat muun muassa tiedon muotoilu, stringien konkatenointi ja suorituskyvyn parantaminen tiedostonkäsittelyssä.

Katso myös:
Jos haluat lisätietoja Fish Shellin tarjoamista toiminnoista, voit tutustua viralliseen dokumentaatioon osoitteessa https://fishshell.com/docs/current/index.html.