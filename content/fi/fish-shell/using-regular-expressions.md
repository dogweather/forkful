---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Fish Shell: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi?

Jos haluat säästää aikaa ja vaivaa tiedostojen ja tietokantojen käsittelyssä, voit käyttää säännöllisiä lausekkeita. Ne ovat tehokkaita työkaluja, jotka auttavat sinua löytämään ja muokkaamaan tietoja tietorakenteista nopeasti ja yksinkertaisesti.

## Kuinka?

```Fish Shell``` tarjoaa kätevän tavan käyttää säännöllisiä lausekkeita suoraan komentoriviltä. Voit esimerkiksi käyttää ```grep``` komentoa etsimään tiettyä merkkijonoa tiedostoista:

```
grep 'helsinki' kaupungit.txt
```

Tämä komento näyttäisi kaikki tiedostot, jotka sisältävät sanan "helsinki". Voit myös käyttää säännöllisiä lausekkeita tiedostojen sisältämien tietojen muokkaamiseen. Esimerkiksi, jos haluat poistaa kaikki numerot sanasta "helsinki", voit käyttää:

```
sed 's/[0-9]//g' kaupungit.txt > uudet_kaupungit.txt
```

Tämä korvaisi kaikki numerot tyhjällä merkillä ja tallentaisi muokatun tiedoston "uudet_kaupungit.txt" nimellä. Voit myös käyttää säännöllisiä lausekkeita verrataksesi erilaisia merkkijonoja ja tehdä päätöksiä sen perusteella. Esimerkiksi, jos haluat tarkistaa onko tietty sana tiedostossa, voit käyttää:

```
if [[ "Helsinki" =~ "helsinki" ]]
    echo 'Tiedostossa on sana "helsinki".'
else
    echo 'Tiedostossa ei ole sanaa "helsinki".'
end
```

## Syvällisemmin

Saadaksesi täyden hyödyn irti säännöllisistä lausekkeista, on tärkeää ymmärtää erilaisia merkkejä ja niiden merkityksiä. Esimerkiksi, voit käyttää ```[]``` merkkejä määrittääksesi erilaisia merkkejä, jotka haluat sisällyttää tai jättää pois. ```^``` merkillä voit lisätä ehtoja, jotka rajatun alueen ulkopuolella voivat aiheuttaa erilaisia toimintoja. Lisäksi, voit käyttää erilaisia kuviorakenteita, kuten ```*``` tai ```+```, toistojen määrän määrittämiseen.

Yksi tapa oppia paremmin säännöllisiä lausekkeita on käyttää niitä käytännössä. Voit löytää tarpeeksi harjoitusta käyttämällä erilaisia komentoriviohjelmia, kuten ```grep```, ```sed``` ja ```awk```, jotka kaikki tukevat säännöllisiä lausekkeita.

## Katso myös

- [Fish Shell virallinen dokumentaatio](https://fishshell.com/docs/current)
- [RegExr](https://regexr.com/), verkossa toimiva säännöllisten lausekkeiden editori
- [Fun with Regular Expressions](https://jvns.ca/blog/2019/08/27/fun-with-regular-expressions/), artikkeli säännöllisten lausekkeiden käytöstä ja leikittelystä