---
title:                "Säännöllisten lausekkeiden käyttö"
date:                  2024-01-19
html_title:           "Bash: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Pythonissa säännölliset lausekkeet, eli regex, ovat tekstinhakutyökalu. Niillä löydetään, tarkastetaan ja muokataan merkkijonoja nopeasti ja tehokkaasti.

## How to:
Pythoniin sisäänrakennettu `re`-moduuli käsittelee regexiä. Tässä pari esimerkkiä.

```Python
import re

# Etsitään kaikki sanat, jotka alkavat 'h' ja päättyvät 'n'
teksti = "hello world, hyvää huomenta, henkilö, huon"
pattern = r'\bh\w*n\b'
matches = re.findall(pattern, teksti)
print(matches)  # Output: ['hen', 'huon']

# Korvataan kaikki numerot tähdillä
teksti = "H4u37t2 5k4iv44t!"
pattern = r'\d'  # \d vastaa numeroita
korvattu_teksti = re.sub(pattern, '*', teksti)
print(korvattu_teksti)  # Output: H*u**t* *k*iv**t!
```

## Deep Dive:
Regexit kehittyivät 1950-luvulla auttamaan merkkijonon käsittelyä. Nykyisin niitä on lähes kaikissa ohjelmointikielissä. Vaihtoehtoina regexille voidaan käyttää esim. Pythonin merkkijonometodeja, mutta ne ovat usein hitaampia monimutkaisemmissa hakutehtävissä. Pythonin `re`-moduulissa käytetään backtracking-algoritmia tehokkuuden vuoksi.

## See Also:
Regexin opetteluun ja testaamiseen:

- Pythonin viralliset `re`-moduulin dokumentit: https://docs.python.org/3/library/re.html
- Regex101, interaktiivinen regex-testaustyökalu: https://regex101.com/
- Pythex, toinen hyvä regex-testeri: https://pythex.org/
