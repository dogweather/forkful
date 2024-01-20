---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Päivämäärän jäsentäminen merkkijonosta tarkoittaa päivämäärätiedon erottamista ja analysointia tekstidatasta. Ohjelmoijat tekevät sen usein datan esikäsittelyssä, jotta he voivat käsitellä päivämääriä matemaattisesti ja tehdä niistä vertailukelpoisia.

## Miten tehdä:
Pythonin standardikirjasto tarjoaa `datetime` moduulin, jossa on apufunktioita päivämäärän jäsentämiseen. Tässä on esimerkki sen käyttämisestä:

```Python
from datetime import datetime

date_string = "15/07/2021"
date_format = "%d/%m/%Y"

try:
    parsed_date = datetime.strptime(date_string, date_format)
    print(parsed_date)
except ValueError:
    print("Incorrect format")
```

Konsoli näyttää seuraavanlaisen tuloksen:

```Python
2021-07-15 00:00:00
```

## Syvä sukellus
Historiallisessa kontekstissa päivämäärän jäsentämistä on käytetty jo vuosikymmeniä, koska kaikki tieto on alkujaan ollut tekstistä. Erilaisia tekniikoita, kuten säännölliset lausekkeet ja leksikaalinen analyysi, on käytetty sen jälkeen.

Vaihtoehtona Pythonille, JavaScript, Java ja monet muut kielet myös tarjoavat työkaluja päivämäärän jäsentämiseen merkkijonosta.

Implementoinnin osalta, `strptime` funktio ottaa sisään kaksi merkkijonoa: päivämäärän tekstiversion ja muotoilumerkkijonon. Funktio selvittää miten päivämäärän tiedot vastaavat muotoilumerkkijonoa ja muodostaa vastaavan `datetime` objektin.

## Katso myös
- [Pythonin virallinen dokumentaatio](https://docs.python.org/3/library/datetime.html)
- [W3Schoolsin päivämäärä- ja aikaoppitunti](https://www.w3schools.com/python/python_datetime.asp)
- [Python datetimesta Stack Overflowssa](https://stackoverflow.com/questions/466345/converting-string-into-datetime)