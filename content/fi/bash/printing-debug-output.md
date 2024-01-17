---
title:                "Tulostaminen virheenkorjauslähtöön"
html_title:           "Bash: Tulostaminen virheenkorjauslähtöön"
simple_title:         "Tulostaminen virheenkorjauslähtöön"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Debug-tulosteiden tulostaminen on prosessi, jolla ohjelmoijat voivat tarkastella koodin suorituksen aikana tapahtuvia tapahtumia ja mahdollisia virheitä. Tämä auttaa heitä korjaamaan koodin ongelmia ja varmistamaan, että ohjelma toimii halutulla tavalla.

## Miten tehdä:
Koodiessa debug-tulosteita Bashissa, käytä komentoa `echo` ja haluamaasi muuttujaa, esimerkiksi `echo $muuttuja`. Tämä tulostaa muuttujan arvon tai viestin näytölle. Voit myös muotoilla tulosteen haluamallasi tavalla lisäämällä liitteitä, kuten `echo "Muuttuja: $muuttuja"`. Debug-tulosteet voivat auttaa sinua tunnistamaan ongelmallisia kohtia koodissa ja seuraamaan ohjelman suoritusta.

## Syväsukellus:
Debug-tulosteet eivät ole ainoa tapa tarkastella koodin suoritusta, ja jotkut ohjelmoijat saattavat mieluummin käyttää vaihtoehtoja, kuten käyttäjän syötteiden tulostamista tai erilaisten työkalujen käyttöä. Debug-tulosteet voivat myös hidastaa ohjelman suoritusta, joten on tärkeää poistaa ne lopullisesta koodista. Voit tehdä tämän käyttämällä ehdollista lausetta, kuten `if [ $debug = true ]`, jotta debug-tulosteet näkyvät vain silloin, kun niitä tarvitaan.

## Katso myös:
Jos olet uusi Bashin käyttäjä tai haluat oppia lisää sen käytöstä, voit tutustua Bashin viralliseen dokumentaatioon osoitteessa https://www.gnu.org/software/bash/. Voit myös lukea lisää debug-tulosteiden käytöstä Bashissa täältä: https://linuxconfig.org/how-to-use-echo-for-debugging-shell-scripts-in-linux.