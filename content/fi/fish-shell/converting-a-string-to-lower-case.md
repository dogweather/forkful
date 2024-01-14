---
title:                "Fish Shell: Muuntaa merkkijono pieniksi kirjaimiksi"
simple_title:         "Muuntaa merkkijono pieniksi kirjaimiksi"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointiprojekteissa voi olla tarpeen muuttaa merkkijono pienaakkosiksi. Tämä voi olla hyödyllistä esimerkiksi vertailujen tekemisessä tai tietokannan käsittelyssä. Fish Shellilla tämä on helppoa tehdä ja se säästää aikaa ja vaivaa.

## Kuinka

Alla on esimerkki koodista, joka muuttaa merkkijonon pienaakkosiksi Fish Shellilla:

```Fish Shell
set string "Hei, tämä On EksaMPle"
set lower_case (string | tr '[:upper:]' '[:lower:]')
echo $lower_case
```

Tämän koodin toiminta selitetään tarkemmin alla olevassa "Deep Dive" osiossa.

Koodin ensimmäisessä rivissä luodaan muuttuja, johon tallennetaan muunnettava merkkijono. Tässä esimerkissä se on "Hei, tämä On EksaMPle".

Toisessa rivissä käytetään ```tr``` komentoa muuttamaan merkkijono pienaakkosiksi. Ensimmäinen argumentti, '[:upper:]', tarkoittaa, että muutetaan kaikki merkkijonon isot kirjaimet pieniksi kirjaimiksi. Toinen argumentti, '[:lower:]', on vastaavasti kaikkien kirjainten muuttamiseksi pieniksi. Tässä vaiheessa muunnettu merkkijono tallennetaan uuteen muuttujaan ```lower_case```.

Viimeisessä rivissä tulostetaan uusi pienaakkoinen merkkijono komennolla ```echo```.

## Deep Dive

Fish Shellin ```tr``` komento on erittäin hyödyllinen työkalu merkkijonojen käsittelyssä. Se ottaa kaksi argumenttia, ensimmäinen on etsittävä merkkijono ja toinen korvaava merkkijono. Käyttämällä '[:upper:]' ja '[:lower:]' argumentteja meidän tapauksessamme, muunnetaan merkkijonon isot kirjaimet pieniksi ja tallennetaan uuteen muuttujaan.

Fish Shellin muuttujien asettelu on myös hyödyllinen taito, jota kannattaa oppia. Muuttuja luodaan komennolla ```set``` ja sitä käytetään myöhemmin komennolla ```echo```.

## Katso myös

- ​[Fish Shellin dokumentaatio](https://fishshell.com/docs/current/index.html)
- ​[Tr-komento käyttöohjeet](https://www.man7.org/linux/man-pages/man1/tr.1.html)