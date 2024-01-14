---
title:                "Python: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi
Python on yksi maailman suosituimmista ohjelmointikielistä, ja sen avulla voi tehdä monenlaisia asioita. Yksi hyödyllisimmistä toiminnoista on tekstin etsiminen ja korvaaminen. Tämä on erityisen hyödyllistä esimerkiksi tekstinkäsittelyohjelmassa tai datan käsittelyssä.

## Kuinka tehdä
Käytä Pythonin "replace()" -metodia etsimään ja korvaamaan tiettyä tekstiä.

```Python
# Luo teksti
teksti = "Tervetuloa kotiin!"

# Käytä replace()-metodia korvataksesi "Tervetuloa" "Hei" -sanalla
uusi_teksti = teksti.replace("Tervetuloa", "Hei")

# Tulosta uusi teksti
print(uusi_teksti)

>>> Hei kotiin!
```

## Syvempi sukellus
Pythonilla voi tehdä tarkempaa tekstien etsimistä ja korvaamista käyttämällä säännöllisiä lausekkeita. Tämä mahdollistaa esimerkiksi useiden erilaisten hakusanojen korvaamisen tai erikoismerkkien huomioimisen.

```Python
# Tuodaan syvempi tekstityökalu
import re

# Luo teksti
teksti = "Tänään on aurinkoinen päivä ja sinne on paljon ihmisiä tulossa."

# Käytä re.sub()-funktiota korvaamaan "sinne" "tänne"
# ja "ihmisiä" "ihmisiäni"
uusi_teksti = re.sub(r'sinne|ihmisiä', lambda x: x.group().capitalize() + 'ni', teksti)

# Tulosta uusi teksti
print(uusi_teksti)

>>> Tänään on aurinkoinen päivä ja tänne on paljon ihmisiäni tulossa.
```

## Katso myös
- [Pythonin dokumentaatio tekstin korvaamisesta](https://docs.python.org/3/library/string.html#string.replace)
- [Regex kirjasto Pythonissa](https://docs.python.org/3/library/re.html)
- [10 hyödyllistä Python-käskyä tekstin käsittelyyn](https://www.pythonforbeginners.com/text-processing/using-regexes-in-python)