---
title:                "Tekstin etsiminen ja korvaaminen"
aliases:
- fi/python/searching-and-replacing-text.md
date:                  2024-01-20T17:58:57.101609-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstin etsiminen ja korvaaminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Hakeminen ja korvaaminen tekstissä tarkoittaa jotakin merkkijonoa toisella korvaamista. Ohjelmoijat tekevät tätä koodin virheitä korjatessaan tai päivittäessään ohjelmiston toimintaa.

## How to: (Kuinka tehdä:)
```Python
# Simppeli hakeminen ja korvaaminen Pythonissa käyttäen str.replace() -metodia.
teksti = "Hello maailma! Tervetuloa ohjelmointiin."
korvattu_teksti = teksti.replace("maailma", "world")
print(korvattu_teksti)
```
Output:
```
Hello world! Tervetuloa ohjelmointiin.
```

## Deep Dive (Sukellus syvälle)
Historiallisesti hakemisen ja korvaamisen konsepti on peräisin tekstinkäsittelyn alkuajoista. Se on ollut oleellinen osa tekstieditoreita, kuten vi ja Emacs, ja myöhemmin integroitunut ohjelmointiympäristöihin. Pythonissa `str.replace()` on yksinkertainen tapa tehdä peruskorvauksia, mutta kun tarvitaan enemmän valtaa ja joustavuutta, `re` moduuli tarjoaa säännöllisten lausekkeiden voiman. Esimerkiksi:

```Python
import re

teksti = "Python 3.6 on vanha versio. Pitäisikö käyttää Python 3.9:tä?"
uusi_teksti = re.sub(r'3\.6', '3.8', teksti)
print(uusi_teksti)
```

Output:
```
Python 3.8 on vanha versio. Pitäisikö käyttää Python 3.9:tä?
```

Säännölliset lausekkeet ovat tehokas, mutta monimutkainen työkalu. Aloittelijoille voi riittää `str.replace()`, mutta pidemmän päälle `re` moduulin opettelu on hyödyllistä.

## See Also (Katso myös)
- Pythonin `str`-metodit: https://docs.python.org/3/library/stdtypes.html#string-methods
- `re` moduulin dokumentaatio: https://docs.python.org/3/library/re.html
- Interaktiivisia säännöllisten lausekkeiden harjoituksia: https://regexr.com/
