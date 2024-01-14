---
title:                "Bash: Testien kirjoittaminen"
programming_language: "Bash"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi kirjoittaa testeja?

Testien kirjoittaminen on tärkeä osa Bash-ohjelmointia, sillä se auttaa varmistamaan, että koodi toimii odotetusti ja vähentää ohjelmistovirheiden riskiä. Se myös auttaa parantamaan koodin laatua ja ylläpidettävyyttä.

## Miten kirjoittaa testeja Bashilla?

Bashin version 4.0 jälkeen se sisältää sisäänrakennetun [[-operaattorin, joka mahdollistaa yksinkertaisten testien kirjoittamisen.

```
#!/bin/bash

# Testataan, muuttuuko muuttuja nimi oikein
nimi="Matti"
if [[ $nimi == "Matti" ]]; then
  echo "Muuttuja nimi on Matti."
else
  echo "Muuttuja nimi ei ole Matti."
fi
```

Tässä esimerkissä [[-operaattori vertaa muuttujaa nimi merkkijonoon "Matti". Jos ehto on tosi, tulostetaan lauseke "Muuttuja nimi on Matti." Jos ehto ei ole tosi, tulostetaan lauseke "Muuttuja nimi ei ole Matti."

Testien kirjoittaminen on erityisen hyödyllistä Bash-skriptejä varten, sillä se auttaa varmistamaan, että skripti toimii odotusten mukaisesti ja havaitsemaan mahdolliset virheet.

## Syvenny testien kirjoittamiseen

Testien kirjoittaminen Bashilla on melko yksinkertaista, mutta on myös mahdollista kirjoittaa monimutkaisempia testejä, jotka tarkistavat esimerkiksi kattavasti skriptin eri osa-alueita.

Lisäksi Bashilla on saatavilla erilaisia testitoimintoja, kuten `test` ja `[` -komento, jotka antavat vielä lisää mahdollisuuksia testien kirjoittamiseen.

On myös hyödyllistä kirjoittaa testejä, jotka testaavat skriptin mahdollisia virhetilanteita ja huonosti muotoiltuja syötteitä. Näin voit varmistaa, että skripti toimii luotettavasti myös näissä tapauksissa.

## Katso myös

- [Bash-käsikirja](https://www.gnu.org/software/bash/manual/html_node/index.html)
- [Bash Testien kirjoittaminen](https://askubuntu.com/questions/521191/how-to-write-a-bash-script-to-test-the-most-frequently-used-command)