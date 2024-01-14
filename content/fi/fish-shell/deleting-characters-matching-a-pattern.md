---
title:    "Fish Shell: Kuviota vastaavien merkkien poistaminen"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluat ehkä poistaa merkkejä, jotka vastaavat tiettyä mallia. Se voi olla osa isompaa skriptiä tai haluat ehkä puhdistaa tietokannan tiettyjen merkkien vuoksi. Fish Shellilla on hyödyllisiä työkaluja, jotka voivat auttaa sinua tässä tehtävässä.

## Miten

Fish Shell tarjoaa erilaisia keinoja poistaa merkkejä, jotka vastaavat tiettyä mallia. Yksi vaihtoehto on käyttää `sed`-komennolla. Voit käyttää seuraavaa syntaksia poistaaksesi kaikki numerot tekstitiedostosta:

```Fish Shell
sed -E 's/[0-9]//g' tiedostonimi.txt
```

Tämä komento korvaa kaikki numerot tyhjillä merkeillä, jolloin ne poistuvat tiedostosta. Voit myös käyttää `tr`-komennolla poistaaksesi tietyt merkit. Esimerkiksi, jos haluat poistaa kaikki välimerkit tekstitiedostosta, voit käyttää seuraavaa:

```Fish Shell
tr -d '[:punct:]' < tiedostonimi.txt
```

Tämä komento poistaa kaikki välimerkit tiedostosta ja tulostaa lopputuloksen näytölle.

## Syväsukellus

`sed`- ja `tr`-komentojen lisäksi Fish Shell tarjoaa myös muita tapoja poistaa merkkejä vastaavat tiettyä mallia. Voit esimerkiksi käyttää `grep`-komennolla löytääksesi tietyn merkkijonon ja sitten poistaa sen `sed`-komennolla. Lisäksi voit käyttää `$string` muuttujaa löytääksesi ja poistaaksesi tietyt merkit.

On myös mahdollista luoda omia funktioita Fish Shellilla, jotka voivat helpottaa merkkien poistamista. Voit luoda oman funktion, joka poistaa kaikki merkit vastaavat tiettyä mallia ja käyttää sitä halutessasi. Mahdollisuudet ovat rajattomat, joten voit löytää juuri sinulle sopivan tavan poistaa merkkejä Fish Shellilla.

## Katso myös

- [Fish Shellin dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Artikkeli merkkijonojen manipuloinnista Fish Shellilla](https://www.digitalocean.com/community/tutorials/how-to-manipulate-strings-in-fish-shell#removing-characters)
- [Ohjeita tekstinkäsittelyyn Fish Shellilla](https://www.shellhacks.com/remove-delete-characters-from-string-bash/)

Kiitos lukemisesta! Toivottavasti tämä opas auttoi sinua poistamaan merkkejä vastaavat tiettyä mallia Fish Shellilla. Jatkossa voit hyödyntää näitä taitoja monissa eri skripteissä ja tehtävissä. Onnea koodaukseen!