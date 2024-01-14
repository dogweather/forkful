---
title:    "Python: Merkkijonon pituuden löytäminen"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi
Usein ohjelmoijat haluavat tietää merkkijonon pituuden tekstitiedostoista, tiedostonimistä tai käyttäjän syötteistä. Tämän tiedon avulla voidaan esimerkiksi tarkistaa, että käyttäjän antama syöte on annettujen rajojen sisällä tai suorittaa tiettyjä toimenpiteitä tietyllä määrällä merkkejä.

## Miten
Pythonissa merkkijonon pituus voidaan selvittää käyttämällä `len()`-funktiota. Esimerkiksi:

```Python
# Määritetään merkkijono
merkkijono = "Tämä on esimerkki"

# Käytetään len()-funktiota
pituus = len(merkkijono)

# Tulostetaan tulos
print(pituus)

# Output: 17
```

## Syventyvä tarkastelu
`len()`-funktio toimii myös muiden tietotyyppien, kuten listojen, kanssa. Se palauttaa kyseisen rakenteen alkioiden määrän.

Lisäksi, jos halutaan laskea merkkijonon pituus ilman välilyöntejä, voidaan käyttää `replace()`-funktiota poistaakseen välilyönnit ennen `len()`-funktion käyttöä.

## Katso myös
- [Pythonin virallinen dokumentaatio](https://docs.python.org/fi/3/library/functions.html#len)
- [Tutorialspointin artikkeli merkkijonojen käsittelystä Pythonissa](https://www.tutorialspoint.com/python/python_strings.htm)