---
title:                "Python: Merkkijonon pituuden etsiminen"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Jokainen, joka on koskaan käyttänyt Pythonia, tietää, että tekstistringsit ovat tärkeä osa ohjelmointia. Mutta miksi meidän pitäisi välittää merkkijonojen pituudesta? Merkkijonojen pituuden laskeminen voi olla hyödyllistä, kun haluamme esimerkiksi tarkistaa, onko käyttäjän syöttämä salasana tarpeeksi pitkä vai laskea kuinka monta kirjainta on tietyn sanan sisällä.

## Kuinka

Merkkijonojen pituuden laskeminen on yksinkertaista Pythonissa. Voimme käyttää *len()* -funktiota. Tämä funktio ottaa yhden argumentin, joka on merkkijono ja palauttaa sen pituuden. Katso esimerkki alla:

```Python
# Määritetään muuttuja tekstistringille
teksti = "Tämä on tekstiä!"

# Kutsutaan len()-funktiota ja tallennetaan sen palauttama arvo muuttujaan
pituus = len(teksti)

# Tulostetaan pituus
print(pituus)
```

Tässä esimerkissä *teksti* -muuttujaan tallennetaan merkkijono "Tämä on tekstiä!" ja sitten käytämme *len()* -funktiota laskemaan sen pituuden. Tämä palauttaa arvon 16, koska merkkijonossa on 16 merkkiä. Voimme myös käyttää *len()* -funktiota suoraan tulostamalla tuloksen ilman, että sitä tallennetaan muuttujaan.

```Python
# Tulostetaan pituus suoraan
print(len("Tämä on tekstiä!"))
```

Tämä tulostaa myös arvon 16.

## Syvemmälle

Merkkijonojen pituuden laskeminen ei rajoitu vain tavanomaisiin stringeihin, vaan voimme käyttää sitä myös listoihin, tupleihin, sanakirjoihin ja jopa tiedostoihin. Lisäksi *len()* -funktiota voidaan käyttää yhdessä muiden funktioiden kanssa, kuten *min()* ja *max()*, jolloin voimme laskea esimerkiksi listan lyhimmän tai pisimmän merkkijonon pituuden.

Ennen kuin käytät *len()* -funktiota, on kuitenkin hyvä tarkistaa, onko se tuetussa muodossa. Esimerkiksi *len()* ei toimi numeroiden kanssa, sillä numerot eivät ole merkkijonoja.

## Katso myös

- [Pythonin dokumentaatio merkkijonojen pituuden laskemisesta](https://docs.python.org/fi/3/library/stdtypes.html#typesseq-common)
- [Tietoa Pythonin *len()* -funktiosta](https://realpython.com/python-length-length-len/)