---
title:                "Työskentely jsonin kanssa"
html_title:           "Python: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/working-with-json.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
JSON eli JavaScript Object Notation on tiedon tallennus- ja siirtoformaatti, jota käytetään usein web-sovellusten kehittämisessä. JSON-rakenteen avulla voidaan tallentaa ja siirtää tietoa ns. avaimen ja arvon parina, mikä tekee tiedon käsittelystä ja tulkinnasta helpompaa. Tämän vuoksi JSON on suosittu tiedon tallennusformaatti ja sitä käytetään laajasti eri ohjelmointikielissä.

## Näin teet:
JSON-tiedon käsittely on helppoa Pythonissa. Moduuli ```json``` tarjoaa valmiit työkalut JSON-tiedon lukemiseen ja kirjoittamiseen. Alla olevassa esimerkissä haetaan JSON-tiedostosta dataa ja tulostetaan se konsoliin.

```python
import json

# luetaan tiedosto ja tallennetaan data-muuttujaan
with open('data.json', 'r') as f:
    data = json.load(f)

# käydään läpi data ja tulostetaan se konsoliin
for item in data:
    print("Nimi:", item["name"])
    print("Ikä:", item["age"])
```

Tulostus oli seuraava:

```
Nimi: Anna
Ikä: 25
Nimi: Mikko
Ikä: 30
```

## Syvemmälle:
JSON syntyi alunperin JavaScript-ohjelmointikielen yhteydessä, mutta sitä voidaan nykyään käyttää monessa muussakin yhteydessä. JSON-tiedoston rakenne muistuttaa paljon Pythonin sanakirjarakennetta, joten se on luonteva tiedon tallennusmuoto Python-kehittäjille.

Jos haluat tarkastella JSON-dataa visuaalisesti, voit käyttää esimerkiksi JSON-lukijaa (viewer), kuten [JSONLint](https://jsonlint.com/). Se auttaa havainnollistamaan JSON-tiedoston rakennetta ja löytämään mahdollisia virheitä.

## Katso myös:
- [Pythonin virallinen dokumentaatio JSON-muotoisen tiedon käsittelystä](https://docs.python.org/3/library/json.html)
- [JSONLint](https://jsonlint.com/) - JSON-lukija ja validointityökalu
- [JSON.org](https://www.json.org/) - JSON-muotoisen tiedon viralliset tiedot