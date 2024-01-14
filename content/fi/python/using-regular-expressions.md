---
title:                "Python: Säännöllisten lausekkeiden käyttö"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Miksi käyttää säännöllisiä lausekkeita? Yksinkertainen vastaus on, että ne ovat voimakas työkalu käsittelemään merkkijonoja ja löytämään tietoa suuresta datamäärästä. Käytännössä säännölliset lausekkeet voivat auttaa sinua löytämään tiettyjä sanoja, numeroita, välimerkkejä tai muita merkkiyhdistelmiä tekstistä. Ne ovat erityisen hyödyllisiä, kun kyseessä on datan käsittely, tekstianalyysi tai tietojen poimiminen websivustoilta.

## Kuinka

Säännöllisten lausekkeiden käyttäminen Pythonissa on helppoa! Aloita tuomalla "re" moduuli koodiisi:

```Python
import re
```

Seuraavaksi voit käyttää "re.search()" funktiota löytääksesi haluamasi merkkijonon tekstistä:

```Python
text = "Tervetuloa Pythonin maailmaan!"
match = re.search("Python", text)
```

"match" sisältää nyt "Python" merkkijonon, jos tekstissä se löytyi. Voit myös käyttää säännöllisiä lausekkeita korvaamaan tai päivittämään merkkijonoja tekstissä:

```Python
text = "Tervetuloa Pythonin maailmaan!"
updated_text = re.sub("maailmaan", "maailma!", text)
```

"updated_text" sisältää nyt päivitetyn tekstin "Tervetuloa Pythonin maailma!"

## Syvemmälle

Voit tehdä paljon enemmän säännöllisillä lausekkeilla kuin vain hakuja ja korvauksia. Ne tarjoavat paljon erilaisia sääntöjä ja ilmaisuja, joilla voit hakea tiettyä tyyppistä dataa. Esimerkiksi voit käyttää "[]" ilmaisua löytääksesi kaikki sanat, jotka alkavat tietyllä kirjaimella:

```Python
text = "Hauskaa matkustamista ja opiskelua!"
match = re.search("[mh]\\w+", text)
```

"match" sisältää nyt "matkustamista" sanan tekstistä.

Säännölliset lausekkeet voivat olla hieman haastavia alussa, mutta harjoituksen myötä voit oppia erilaisia ilmaisuja ja taitojasi käyttää niitä taitavasti.

## Katso myös

Tässä on muutama hyödyllinen linkki lisätietoja ja harjoituksia varten säännöllisten lausekkeiden käytöstä Pythonissa:

- [W3Schools: Regular Expressions in Python](https://www.w3schools.com/python/python_regex.asp)
- [Real Python: Regular Expressions in Python - Practical Introduction](https://realpython.com/regex-python/)
- [Python for Beginners: Regex Tutorial](https://www.pythonforengineers.com/regex-tutorial/)

Nyt olet valmis astumaan säännöllisten lausekkeiden maailmaan. Onnea ja paljon koodausiloa!