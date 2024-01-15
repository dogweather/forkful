---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Python: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Syytä etsiä ja korvata tekstiä voi olla monia, kuten halu muuttaa tietyn sanan tai lauseen jokaisen esiintymän tai korjata kirjoitusvirheitä.

## Miten

Etsi ja korvaa tekstiä Pythonilla on helppoa! Käytämme tähän replace() -funktiota, joka toimii seuraavalla tavalla:

```Python 
# Alkuperäinen teksti
teksti = "Tämä on esimerkkiteksti."

# Korvataan sana "esimerkki" sanoilla "loistava"
uusi_teksti = teksti.replace("esimerkki", "loistava")

# Tulostetaan uusi teksti
print(uusi_teksti)

# Output: Tämä on loistava teksti.
```

Yllä olevassa esimerkissä olemme ensin tallentaneet alkuperäisen tekstin muuttujaan "teksti". Sitten replace() -funktion avulla olemme korvanneet sanan "esimerkki" sanalla "loistava" ja lopuksi tulostaneet uuden tekstimme. Voit myös vaihtaa vanhan tekstin uuteen muuttamalla sanat ja tekemällä tarvittavat muutokset. 

## Syvemmälle

Replace() -funktio toimii myös kirjaimien ja numeroiden kanssa. Esimerkiksi voit korvata kaikki pienet kirjaimet isoilla kirjaimilla tai poistaa numerot tekstistäsi. Voit myös antaa vaihtoehdon, kuinka monta kertaa tekstiä korvataan. Jos haluat korvata kaikki esiintymät, voit käyttää rstrip() -funktiota. Tarkempia tietoja ja esimerkkejä löydät Pythonin virallisesta dokumentaatiosta.

## Katso myös

- [Pythonin virallinen dokumentaatio](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Tutoriaali videoiden ja esimerkkien kanssa](https://www.youtube.com/watch?v=qEYZDm6vPCQ)
- [Githubin hakutyökalut](https://www.youtube.com/watch?v=yxXX1aQaAKU)