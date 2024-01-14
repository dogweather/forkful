---
title:    "Python: Stringien yhdistäminen"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluat yhdistää merkkijonoja Pythonilla? Joillakin ohjelmointitehtävillä on tarpeellista yhdistää merkkijonoja luomaan uusia, pidempiä merkkijonoja esimerkiksi tulosteiden tai käyttäjän syötteiden luomiseksi.

## Miten

Yhdistämisen suorittaminen Pythonilla on helppoa käyttämällä `+` -operaattoria. Katso alla olevia esimerkkejä käyttötapauksista ja odotettuja tulosteita.

```Python
# Yksinkertainen merkkijonon yhdistäminen
nimi = "Matti"
tervehdys = "Hei " + nimi
print(tervehdys)
# TULOSTUS: Hei Matti

# Yhdistäminen numeron kanssa
luku = 42
vastaus = "Universumin vastaus on " + str(luku)
print(vastaus)
# TULOSTUS: Universumin vastaus on 42

# Käyttäjän syötteen yhdistäminen
lempiväri = input("Mikä on lempivärisi? ")
print("Minun lempivärini on " + lempiväri + " myös!")
# KÄYTTÄJÄN SYÖTE: Sininen
# TULOSTUS: Minun lempivärini on Sininen myös!
```

## Syvällisempi tarkastelu

Pythonin sisäänrakennettu `+` -operaattori toimii concatenation (yhdistäminen) nimisenä prosessina, jossa kaksi merkkijonoa yhdistetään yhdeksi kokonaisuudeksi. Tämä on erittäin hyödyllinen tapa luoda käyttäjän syötteitä, mallisanoja ja monia muita ohjelmointitehtäviä.

## Katso myös

- [Pythonin merkkijonojen dokumentaatio](https://docs.python.org/3/library/string.html)
- [Pythonin perusoperaattorit](https://docs.python.org/3/library/operator.html)