---
title:                "Merkkijonon interpolointi"
html_title:           "Python: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

Mitä se on & miksi?: 
Merkkijonon interpolointi tarkoittaa merkkijonon muotoilun lisäämistä muuttujien tai muiden arvojen kanssa samaisen merkkijonon sisällä. Tämä on erittäin hyödyllinen tapa muokata merkkijonoja ja yhdistää erilaisia tietoja helposti.

Miten?: 
Esimerkiksi, jos haluat tulostaa tervehdyksen muuttujan kanssa, voit käyttää merkkijonon interpolointia sijoittamalla muuttujan arvon halutun merkkijonon sisälle. Alla on yksinkertainen esimerkki:

```Python
nimi = "Matti"
print("Hei {nimi}, tervetuloa Pythonin maailmaan!")
```

Tämä tuottaisi seuraavan tulosteen:

```Python
Hei Matti, tervetuloa Pythonin maailmaan!
```

Deep Dive:
Merkkijonon interpolointi ei ole uusi käsite, vaan sitä on käytetty ohjelmointimaailmassa jo pitkään. Aiemmin sitä kutsuttiin muotoillun merkkijonon käyttämiseksi, mutta nykyään tätä termiä ei enää yleisesti käytetä.

On olemassa myös muita tapoja muotoilla merkkijonoja, kuten käyttämällä %-operaattoria tai format()-funktiota. Nämä ovat vaihtoehtoisia tapoja saavuttaa sama tulos kuin merkkijonon interpoloinnilla. Pythonin uudemmat versiot suosittavat kuitenkin merkkijonon interpolointia sen selkeyden ja luettavuuden vuoksi.

See Also:
Mikäli haluat tutustua merkkijonon interpolointiin tarkemmin, suosittelemme lukemaan Pythonin virallisen dokumentaation aiheesta: https://docs.python.org/3/library/string.html#format-string-syntax.

Tutustu myös muihin tapoihin muotoilla merkkijonoja, kuten %-operaattoriin ja format()-funktioon. Näitä käsitellään tarkemmin tässä artikkelissa: https://realpython.com/python-string-formatting/.