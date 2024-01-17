---
title:                "Merkkijonon kirjoitusasu"
html_title:           "Python: Merkkijonon kirjoitusasu"
simple_title:         "Merkkijonon kirjoitusasu"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Kapitalisoiminen on tapa muuntaa merkkijono alkamaan isolla kirjaimella ja muut kohdat säilyttävät pienet kirjaimet. Tätä käytetään usein tekstin muokkauksessa, esimerkiksi otsikoiden muotoilussa. Ohjelmoijat käyttävät tätä toimintoa helpottaakseen tekstin muokkausta ja käsittelemistä.

## Kuinka tehdä se?

```Python
teksti = "esimerkki teksti"
print(teksti.capitalize())
```

Tulostus:
```
Esimerkki teksti
```

Voimme myös kapitalisoida vain tietyn kohdan merkkijonosta käyttämällä ```title()``` metodia.

```Python
teksti = "esimerkki teksti"
print(teksti.title())
```

Tulostus:
```
Esimerkki Teksti
```

## Syväsukellus

Kapitalisointiin liittyvät käytännöt juontavat juurensa fyysisistä kirjoituskoneista, joissa kirjaimien koon muuttaminen oli mahdotonta. Nykyään kapitalisointia käytetään paljon tekstinkäsittelyohjelmissa ja ohjelmoijat käyttävät sitä yksinkertaisemman ja yhtenäisen koodin kirjoittamiseen.

On olemassa myös muita tapoja kapitalisoida merkkijonoja, kuten "upper()" ja "lower()" metodit, jotka muuttavat kaikki kirjaimet isiksi tai pieniksi.

## Katso myös

- [Pythonin virallinen dokumentaatio kapitalisoinnista](https://docs.python.org/3/library/stdtypes.html#str.capitalize)
- [Erilaisia tapoja käsitellä merkkijonoja Pythonilla](https://www.tutorialspoint.com/python3/python_strings.htm)