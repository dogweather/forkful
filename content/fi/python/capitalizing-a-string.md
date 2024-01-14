---
title:    "Python: Merkkijonon suuruuskirjainten muuttaminen"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Miksi

Miksi ja milloin sitaisiin päättää käyttää merkkijonon isoja alkukirjaimia koodissa?

## Miten

Jos haluat muuttaa merkkijonon alkukirjaimet isoiksi, voit käyttää Pythonin .capitalize() -metodia. Se tarkoittaa, että ensimmäisen sanan ensimmäinen kirjain muutetaan isolle mieleksyppäjälle. Jos merkkijono on kaikki isoilla, mitään muutosta ei tapahdu. Katso alla oleva esimerkki:

```Python
merkkijono = "tämä on esimerkkilause."
print(merkkijono.capitalize())
```

Tulos:
"Tämä on esimerkkilause."

## Syväsukellus

Pythonin .capitalize() -metodi on hyödyllinen, kun haluat muuttaa merkkijonon ulkoasua, esimerkiksi tekstin muotoilua. Se ei kuitenkaan toimi, jos merkkijonossa on jo erityisiä merkkejä, kuten välilyöntejä tai symboleja. Tämän vuoksi on myös hyödyllistä käyttää muita merkkijonojen muokkausmenetelmiä, kuten .lower(), .upper() ja .strip().

## Katso myös

- [Pythonin merkkijonot ja niiden manipulointi](https://realpython.com/python-strings/)
- [Pythonin merkkijonon muokkausmetodit](https://www.w3schools.com/python/python_ref_string.asp)