---
title:                "Python: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää regular expressioneja?

Regular expressionit (regular expressions) ovat voimakkaita työkaluja, joiden avulla voit tehokkaasti suodattaa, hakea ja muokata tekstiä Pythonilla. Ne ovat erityisen hyödyllisiä, kun haluat etsiä tiettyjä sanoja tai lausekkeita suuresta tekstimäärästä. Jos olet kiinnostunut tekstianalyysistä tai datan käsittelystä, regular expressionit ovat ehdottomasti tutustumisen arvoinen.

## Kuinka käyttää regular expressioneja Pythonissa

Regular expressionien käyttäminen Pythonissa on helppoa. Voit ensin tuoda tarvittavan "re" -kirjaston käyttöön:

```Python
import re 
```

Seuraavaksi voit määrittää haluamasi regular expressionin ja tallentaa sen muuttujaan:

```Python
pattern = r"maito"
```

Jos haluat nähdä, kuinka monta kertaa "maito" esiintyy tiedostossa, voit käyttää "re.findall" -funktiota ja antaa sille parametrina muuttujan ja käsiteltävän tiedoston nimen:

```Python
matches = re.findall(pattern, "ostin maitoa kaupasta tänään")
print(matches)
```

Tämä tulostaa: ["maito"]. Jos haluat hakea eri kirjoitusmuotoja, voit käyttää "flags" -parametria, kuten alla olevassa esimerkissä:

```Python
pattern = r"maito"
matches = re.findall(pattern, "Ostin MAITOA kaupasta tänään", flags = re.IGNORECASE)
print(matches)
```

Tämä tulostaa edelleen ["maito"], vaikka "MAITOA" onkin kirjoitettu suurilla kirjaimilla.

## Syväsukellus regular expressioneihin

Regular expressioneilla on monia eri käyttötapoja, ja niitä voi hyödyntää erilaisissa ohjelmointitehtävissä. Voit esimerkiksi käyttää niitä luomaan omia tekstintyöstötoimintoja, kuten sanojen korvaamista tai tekstien jäsennystä. On myös mahdollista luoda monimutkaisempia regular expressioneja, jotka pystyvät tunnistamaan esimerkiksi tietyn muotoiset sähköpostiosoitteet tai puhelinnumerot.

On kuitenkin tärkeää muistaa, että regular expressioneja ei tulisi käyttää ainoana tapana tietojen käsittelyssä, vaan ne tulisi yhdistää muihin työkaluihin, kuten merkkijonon manipulointimetodeihin.

## Katso myös
- Pythonin virallinen "re" -kirjasto: https://docs.python.org/3/library/re.html
- Regular expressionien opetusvideo: https://www.youtube.com/watch?v=ZdDOauFIDkw&ab_channel=freeCodeCamp.org
- Harjoitustehtäviä regular expressioneihin: https://regexone.com/