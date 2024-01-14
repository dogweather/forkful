---
title:                "Python: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Useimmissa ohjelmointikielissä on toiminto muuttaa merkkijono pieniksi kirjaimiksi. Tämä on kätevä toiminto, jos haluat vertailla merkkijonoja ilman, että niiden kirjainkoko vaikuttaa vertailuun. Esimerkiksi tietokantaohjelmissa tai tekstinkäsittelyssä tämä toiminto on erittäin hyödyllinen.

## Miten

Käytännössä merkkijonon muuttaminen pieniksi kirjaimiksi Pythonissa on erittäin helppoa. Tässä on muutama esimerkki ja niiden tulosteet:

```
Python
sana = "TÄMÄ ON MERKKIJONON MUUTTAMINEN PIENIKSI KIRJAIMIKSI"
print(sana.lower())
```

Tuloste: "tämä on merkkijonon muuttaminen pieniksi kirjaimiksi"

```
Python
lause = "Tämä on esimerkki lauseesta"
print(lause.lower())
```

Tuloste: "tämä on esimerkki lauseesta"

```
Python
teksti = "123456789"
print(teksti.lower())
```

Tuloste: "123456789"

Kuten näet, funktion käyttäminen ei ole riippuvainen merkkijonon pituudesta tai sen sisällöstä. Se muuttaa vain kaikki kirjaimet pieniksi.

## Syvä sukellus

Tarkemmin sanottuna funktion `lower()` toiminta riippuu käytetystä kielestä. Esimerkiksi suomen kielessä ei ole eroa isoilla ja pienillä kirjaimilla, joten funktion `lower()` käyttö ei ole tarpeellista. 

Toinen tärkeä seikka on, että `lower()` ei muuta merkkijonoa pysyvästi. Se palauttaa uuden merkkijonon, jossa kaikki kirjaimet ovat pieniä. Tämä tarkoittaa sitä, että alkuperäinen merkkijono säilyy muuttumattomana, ellei siihen tehdä uutta muutosta.

## Katso myös

- Pythonin dokumentaatio: https://docs.python.org/3/library/stdtypes.html#str.lower
- Stack Overflow: https://stackoverflow.com/questions/6797984/how-to-convert-string-to-lowercase-in-python