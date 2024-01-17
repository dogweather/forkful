---
title:                "Tiedoston lukeminen"
html_title:           "Python: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Mikä ja Miksi?

Tekstitiedoston lukeminen on prosessi, jossa ohjelma lukee sisältöä tiedostosta ja käsittelee sitä. Tämä on tärkeä tehtävä monille ohjelmoijille, sillä tekstitiedostot ovat yleinen tiedostomuoto tietojen tallentamiseen ja jakamiseen.

# Kuinka toimia:

```Python
# Avaa tiedosto "tekstitiedosto.txt"
f = open("tekstitiedosto.txt", "r")

# Lue tiedoston sisältö ja tallenna se muuttujaan "sisalto"
sisalto = f.read()

# Tulosta sisältö
print(sisalto)

# Sulje tiedosto kun olet valmis
f.close()
```

Tämä koodi avaa tekstitiedoston nimeltä "tekstitiedosto.txt" ja tallentaa sen sisällön muuttujaan. Sitten se tulostaa tiedoston sisällön ja lopuksi sulkee tiedoston. Tämän ansiosta voit lukea ja käsitellä tekstitiedoston sisältöä ohjelmassasi.

# Syvällinen sukellus:

Historiallisessa kontekstissa tekstitiedostojen lukeminen on ollut tärkeä osa tietokoneohjelmointia jo pitkään. Ennen graafisia käyttöliittymiä tekstitiedostot olivat yleisin tapa tallentaa ja jakaa tietoa tietokoneiden välillä.

Vaikka tekstitiedostot ovat edelleen suosittu tiedostomuoto, on olemassa myös muita tapoja lukea tiedostojen sisältöä ohjelmassa. Esimerkiksi CSV-tiedostot (comma-separated values) ovat hyvä vaihtoehto, jos haluat käsitellä taulukkomuotoisia tietoja.

Tekstitiedostojen lukeminen tapahtuu yleensä avulla "file" -olioita, jotka tarjoavat joukon käteviä metodeja tiedostojen käsittelyyn. Voit esimerkiksi käyttää "read()" -metodia lukeaksesi tiedoston sisällön tai "close()" -metodia sulkeaksesi tiedoston.

#Katso myös:

Tässä artikkelissa esiteltyjä asioita kannattaa tutkia lisää Pythonin virallisesta dokumentaatiosta, jossa löytyy kattavat ohjeet ja esimerkit tiedostojen käsittelyyn. Voit myös kokeilla lukemista muista tiedostomuodoista kuten CSV:stä tai JSON:ista ja nähdä miten ne eroavat tekstitiedostoista.