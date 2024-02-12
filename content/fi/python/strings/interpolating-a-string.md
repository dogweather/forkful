---
title:                "Merkkijonon interpolointi"
aliases: - /fi/python/interpolating-a-string.md
date:                  2024-01-28T21:24:07.519332-07:00
model:                 gpt-4-0125-preview
simple_title:         "Merkkijonon interpolointi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/interpolating-a-string.md"
changelog:
  - 2024-01-28, dogweather, reviewed
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Merkkijonon interpolaatio on menetelmä, jossa lausekkeita sisällytetään osaksi merkkijonoliteraaleja. Ohjelmoijat käyttävät sitä dynaamisesti arvojen lisäämiseen merkkijonoihin, mikä tekee koodista luettavampaa ja siistimpää kuin perinteinen merkkijonojen yhdistäminen.

## Miten:
Python 3.6:ssa ja sitä uudemmissa versioissa voit interpoloida merkkijonoja käyttämällä f-merkkijonoja. Näin se tehdään:

```Python
name = 'Alice'
age = 30
greeting = f"Hei, {name}. Olet {age} vuotta vanha."

print(greeting)
```

Tuloste:
```
Hei, Alice. Olet 30 vuotta vanha.
```

Voit myös käyttää lausekkeita aaltosulkujen sisällä:

```Python
a = 5
b = 10
info = f"Viisi plus kymmenen on {a + b}, ei {2 * (a + b)}."

print(info)
```

Tuloste:
```
Viisi plus kymmenen on 15, ei 30.
```

## Syväluotaus
Ennen Python 3.6:a `.format()` oli tapa tehdä merkkijonon interpolaatiota:

```Python
name = 'Bob'
age = 25
greeting = "Hei, {}. Olet {} vuotta vanha.".format(name, age)

print(greeting)
```

Vanhan koulukunnan Pythonissa (versiot < 2.6) käytettiin `%`-operaattoria interpolaatioon, mikä on vähemmän intuitiivista ja voi muuttua sekavaksi useiden muuttujien kanssa:

```Python
name = 'Carol'
age = 35
greeting = "Hei, %s. Olet %d vuotta vanha." % (name, age)

print(greeting)
```

Siistimmän syntaksin lisäksi f-merkkijonot ovat nopeampia, koska ne arvioidaan suoritusaikana ja muunnetaan sitten suoraan tehokkaaseen merkkijonoformaattitoimintoon. `.format()`-metodi ja `%`-operaattori sisältävät useampia vaiheita ja ovat hitaampia.

## Katso Myös
- [PEP 498 – Kirjaimellinen merkkijonon interpolaatio](https://www.python.org/dev/peps/pep-0498/) virallisessa dokumentaatiossa f-merkkijonoista.
- [Python f-merkkijonot](https://realpython.com/python-f-strings/) Real Pythonin opastuksessa f-merkkijonojen käyttöön.
- [.format()-Metodi](https://docs.python.org/3/library/stdtypes.html#str.format) Pythonin dokumentaatiossa ymmärtämään vanhempaa `.format()`-menetelmää merkkijonojen muotoiluun.
