---
title:                "Python: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi lukea tekstitiedostoja?

Lukeminen on yksi tärkeimmistä taidoista, jota tarvitset koodatessasi Pythonilla. Monet koodaustehtävät vaativat tiedon lukemista ja käsittelyä tekstitiedostosta, joten tämän taidon hallitseminen on välttämätöntä.

## Kuinka lukea tekstitiedostoja?

Tekstitiedostojen lukeminen Pythonilla on hyvin yksinkertaista. Voit käyttää `open()`-funktiota, joka avaa tiedoston ja palauttaa tiedostopointterin. Tämän jälkeen voit käyttää `read()`-metodia lukeaksesi tiedoston sisällön ja tallentaa sen muuttujaan. Esimerkiksi:

```python
file = open("tekstitiedosto.txt", "r")
sisalto = file.read()
print(sisalto)
```

Tämä koodi avaa tekstitiedoston nimeltä "tekstitiedosto.txt" ja tallentaa sen sisällön muuttujaan `sisalto`. Lopuksi tulostetaan tiedoston sisältö konsolille. Huomaa, että `"r"`-parametri avaukseen määrittää, että tiedosto avataan lukutilassa.

## Syvällinen sukellus

Pythonilla on monia tapoja lukea ja käsitellä tekstitiedostoja, kuten käyttämällä `with`-lauseketta tai `readline()`-metodia. Voit myös määrittää avaukseen erilaisia parametreja, kuten `"w"`-parametrin kirjoitustilassa avaamiseen. Lisää tietoa näistä vaihtoehdoista löydät Pythonin virallisesta dokumentaatiosta.

Lisäksi Pythonilla on myös erilaisia kirjastoja, kuten `csv` ja `json`, jotka tekevät tekstitiedostojen lukemisesta ja käsittelystä vieläkin helpompaa. Nämä kirjastot tarjoavat käteviä toimintoja tiedostojen parsimiseen ja datan muuttamiseen eri muotoihin.

## Katso myös

- [Pythonin virallinen dokumentaatio](https://docs.python.org/3/tutorial/inputoutput.html)
- [CSV-kirjaston dokumentaatio](https://docs.python.org/3/library/csv.html)
- [JSON-kirjaston dokumentaatio](https://docs.python.org/3/library/json.html)