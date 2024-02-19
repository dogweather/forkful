---
aliases:
- /fi/bash/extracting-substrings/
date: 2024-01-20 17:45:03.252593-07:00
description: "Substringien erottelu tarkoittaa isomman merkkijonon osan leikkaamista\
  \ ja k\xE4ytt\xE4mist\xE4. Ohjelmoijat tekev\xE4t t\xE4t\xE4, kun tarvitsevat t\xF6\
  ihin vain osan datasta."
lastmod: 2024-02-18 23:09:07.790507
model: gpt-4-1106-preview
summary: "Substringien erottelu tarkoittaa isomman merkkijonon osan leikkaamista ja\
  \ k\xE4ytt\xE4mist\xE4. Ohjelmoijat tekev\xE4t t\xE4t\xE4, kun tarvitsevat t\xF6\
  ihin vain osan datasta."
title: Merkkijonojen osien poimiminen
---

{{< edit_this_page >}}

## What & Why - Mitä ja Miksi?
Substringien erottelu tarkoittaa isomman merkkijonon osan leikkaamista ja käyttämistä. Ohjelmoijat tekevät tätä, kun tarvitsevat töihin vain osan datasta.

## How to - Kuinka tehdään
```Bash
#!/bin/bash
# Määritellään merkkijono
koko_jono="Hello, Terve Maailma!"

# Erotetaan alijono käyttäen leikkausparametrejä
ali_jono="${koko_jono:7:6}"

# Tulostetaan alijono
echo "$ali_jono"
```

Sample output:
```
Terve
```

## Deep Dive - Syväsukellus
Bash on ollut käytössä jo vuosikymmeniä. Alkuaikoina, merkkijonojen käsittely oli rajoitetumpaa, mutta nykyisin Bash tukee monipuolisesti substrings. Vaihtoehtona voi käyttää `awk`, `sed`, tai `cut`. Tarkasti katsottuna, Bash-tekniset yksityiskohdat riippuvat siitä, mitä shell-versiota käytetään. Uusimmissa versioissa käyttäjät voivat leikata alijonoja suoraan ilman ulkopuolisia ohjelmia.

## See Also - Katso Myös
1. Bash-hahmomuunnokset – `man bash` ja etsi "Parameter Expansion".
2. Linux `cut` komento – hyvä vertailupiste Bashin omille toiminnoille.
3. Advanced Bash-Scripting Guide – syvällisempää tietoa skriptauksesta.
