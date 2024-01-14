---
title:                "Python: Mallia vastaavien merkkien poistaminen."
simple_title:         "Mallia vastaavien merkkien poistaminen."
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Monissa tilanteissa saattaa olla tarpeellista poistaa merkkejä, jotka vastaavat tiettyä kaavaa. Tämä voi auttaa esimerkiksi siivoamaan dataa tai työskentelemään tietyntyyppisissä tekstissä. Seuraavassa kirjoituksessa tutustutaan, kuinka tämä onnistuu Pythonin avulla.

## Kuinka

Poistaaksesi merkit, jotka vastaavat tiettyä kaavaa, voit käyttää Pythonin sisäänrakennettua "re" -moduulia. Tämä moduuli tarjoaa työkalut säännöllisten lausekkeiden käsittelemiseen, mikä on juuri mitä tarvitsemme tässä tapauksessa. Alla on esimerkki koodista, joka poistaa kaikki numerot kokonaan merkkijonosta ja tulostaa lopputuloksen:

```python
import re

teksti = "Tervetuloa käyttämään 123 Pythonia!"
puhdas_teksti = re.sub(r"\d+", "", teksti)
print(puhdas_teksti)

# Tulostaa: Tervetuloa käyttämään Pythonia!
```

Koodissa on ensin tuotu "re" -moduuli "import" -avainsanalla. Sitten alustetaan muuttuja, joka sisältää tekstirivin, josta haluamme poistaa numerot. Käytämme "re" -moduulin "sub" -funktiota, jonka avulla voimme korvata kaavan vastaavat merkit tyhjällä merkkijonolla. Funktioon annetaan ensimmäisenä parametrina kaava, joka etsitään tekstistä, ja toisena parametrina se, millä haluamme korvata kaavan vastaavat merkit. Lopuksi tulostetaan muokattu teksti. Huomaa, että etumerkki "r" tekstin edessä osoittaa, että käytämme raakatekstiä, mikä estää erikoismerkkien, kuten "\", muuttumisen.

## Syvällisempi tarkastelu

"Säännölliset lausekkeet" tai "regulaariset lausekkeet" ovat tapa ilmaista kaavoja, joiden avulla voi etsiä ja muokata merkkijonoja halutulla tavalla. Ne sisältävät erilaisia sääntöjä ja erikoismerkkejä, jotka auttavat löytämään tietynlaisia merkkijonoja. Esimerkiksi yllä käyttämämme kaava "\d+" tarkoittaa yhden tai useamman numeron löytämistä.

Säännölliset lausekkeet voivat joskus vaikuttaa hankalilta aluksi, mutta kun opettelee niiden perusteet ja ymmärtää niiden rakenteen, ne voivat olla todella hyödyllisiä työkaluja. Suosittelemme tutustumaan lisää säännöllisiin lausekkeisiin ja niiden käyttöön, sillä niitä voi hyödyntää monissa eri tilanteissa Python-ohjelmoinnissa.

## Muista tutustua myös

Olemme nyt käyneet läpi, kuinka voit poistaa merkkejä, jotka vastaavat tiettyä kaavaa, Pythonin avulla. Mikäli haluat syvällisempää tietoa säännöllisistä lausekkeista, suosittelemme tutustumaan alla oleviin linkkeihin:

- [Pythonin dokumentaatio "re" -moduulista](https://docs.python.org/3/library/re.html)
- [RegExr - säännöllisten lausekkeiden testaustyökalu](https://regexr.com/)
- [GeeksforGeeks - opetusohjelma säännöllisistä lausekkeista Python