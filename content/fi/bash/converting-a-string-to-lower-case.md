---
title:                "Muuntaa merkkijono pienaakkosiksi"
html_title:           "Bash: Muuntaa merkkijono pienaakkosiksi"
simple_title:         "Muuntaa merkkijono pienaakkosiksi"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Usein Bashissa tarvitaan merkkijonon muokkausta, ja toisinaan halutaan muuttaa kaikki kirjaimet pieniksi. Tässä artikkelissa näytämme, kuinka muuttaa merkkijono pieniksi kirjaimiksi yhdellä komennolla.

## Kuinka

Merkkijonon muuttaminen pieniksi kirjaimiksi on helppoa käyttämällä Bashin sisäänrakennettua `tr` komentoa. Komento ottaa vastaan kaksi parametria: "vanhat" ja "uudet" merkit, ja vaihtaa kaikki annetun merkkijonon kirjaimet vastaaviin uusiin merkkeihin.

```Bash
tr "A-Z" "a-z" <<< "THIS IS A TEST"
```

Tämä komento tulostaa merkkijonon "this is a test". Komento "A-Z" määrittelee kaikki isot kirjaimet vanhoiksi merkeiksi, ja "a-z" määrittelee kaikki pienet kirjaimet uusiksi merkeiksi. Näin kaikki isot kirjaimet korvataan pienillä kirjaimilla.

Jos haluat suojata joitakin merkkejä muuttamiselta, voit käyttää lisäksi `-C` komentoa. Tämä komento antaa lisämerkkejä, jotka pysyvät muuttumattomina. Esimerkiksi, jos haluat säilyttää välilyöntejä muuttumattomina, käytä seuraavaa komentoa:

```Bash
tr "A-Z" "a-z" -C " " <<< "THIS IS A TEST"
```

Tämä tulostaa saman merkkijonon kuin edellinen komento, mutta välilyönnit pysyvät muuttumattomina.

## Syväsyventymine

`tr` komento on yksinkertainen ja tehokas vaihtoehto merkkijonon muokkaamiseen. Se on hyödyllinen esimerkiksi erilaisten tietojen käsittelyssä komentorivillä ja skripteissä. Voit myös käyttää muita säännöllisiä lausekkeita `tr` komennossa muokatakseen merkkijonoa haluamallasi tavalla.

## Katso myös

- [Bashin manuaalisivu](https://man7.org/linux/man-pages/man1/bash.1.html)
- [Regular Expression Tutorial](https://www.regular-expressions.info/tutorial.html)