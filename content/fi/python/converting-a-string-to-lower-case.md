---
title:    "Python: Merkkijonon muuttaminen pieniksi kirjaimiksi"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmointitehtävänä voi olla muuttaa merkkijono pienaakkosiksi, esimerkiksi käyttäjän antaman syötteen yhtenäistämiseksi. Tämä on myös tärkeää, jos halutaan vertailla merkkijonoja, koska pienellä ja isolla alkukirjaimella voi olla eroa merkityksessä.

## Kuinka

Käyttäen Pythonin string-metodia `lower()`, voimme muuttaa merkkijonon pienaakkosiksi. Alla on esimerkkejä miten tämä toimii:

```Python
merkkijono = "TÄMÄ ON MERKKIJONO"
print(merkkijono.lower())
# Output: tämä on merkkijono

merkkijono2 = "EiNiUsKa"
print(merkkijono2.lower())
# Output: einiuska
```

## Syvällisemmin

Pythonin string-metodi `lower()` toimii muuttamalla jokaisen merkin alakerrimeksi, jos se on iso kirjain. Tämä tapahtuu käyttämällä ASCII-taulukkoa, jolla jokaisella kirjaimella on oma numerollinen esitys. Isolla ja pienellä kirjaimella on usein eri numerollinen esitys, joten `lower()`-metodi muuttaa sen vastaavaan pienaakkoseen.

## Katso myös

- [Pythonin string-metodit](https://docs.python.org/fi/3/library/stdtypes.html#string-methods)
- [ASCII-taulukko](https://fi.wikipedia.org/wiki/ASCII)