---
title:                "Merkkijonon muuntaminen pienaakkosiksi"
html_title:           "Gleam: Merkkijonon muuntaminen pienaakkosiksi"
simple_title:         "Merkkijonon muuntaminen pienaakkosiksi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Mikä on merkkijonon muuntaminen pieniksi kirjaimiksi ja miksi ohjelmoijat tekevät sitä? 

Merkkijonon muuntaminen pieniksi kirjaimiksi tarkoittaa, että muutetaan merkkijonon kaikki kirjaimet pieniksi kirjaimiksi. Tätä tehdään usein jotta voidaan vertailla merkkijonoja, kun halutaan tarkastella niiden kirjainkoosta riippumatta.

Ohjelmoijat tekevät tätä usein siksi, että heidän ohjelmansa toimisi oikein kaikissa tapauksissa. Jos ohjelmassa käytetään esimerkiksi käyttäjän antamaa merkkijonoa, ei voi tietää millaisia kirjaimia se sisältää ja siksi on varmempi muuttaa kaikki kirjaimet pieniksi, jotta vältytään mahdollisilta ongelmilta.

## Kuinka: 

````Gleam
let s = "HeLLo WoRlD"
gleam_string.to_lower(s)
````

````Gleam 
= "hello world"
````

Ensiksi luodaan muuttuja, johon tallennetaan haluttu merkkijono. Sitten käytetään Gleamin to_lower funktiota, jolloin merkkijono muutetaan pieniksi kirjaimiksi. Lopuksi tulostetaan uusi merkkijono, joka sisältää vain pieniä kirjaimia.

## Syvemmälle:

Merkkijonon muuntaminen pieniksi kirjaimiksi on tehokas tapa vertailla merkkijonoja, sillä kirjainten koolla ei ole enää väliä. Tämä toiminto antaa myös tasapuolisen lopputuloksen, jos merkkijonossa on eri kirjainkokoja.

Toinen tapa muuntaa merkkijono pieniksi kirjaimiksi on käyttää for loopia ja muuttaa kirjaimet pieniksi yksi kerrallaan. Tämä saattaa kuitenkin olla tehottomampi ratkaisu joissain tapauksissa.

Gleamin to_lower funktio käyttää Unicode-taulukkoa muuntamisen toteuttamiseen, joten se pystyy käsittelemään myös erikoismerkkejä ja merkkejä eri kielistä. Tämä tekee Gleamista hyvän ratkaisun monikielisiin ohjelmiin.

## Katso myös:

Viimeisimmät dokumentit ja ohjeet löytyvät osoitteesta [gleam.run/docs](https://gleam.run/docs).