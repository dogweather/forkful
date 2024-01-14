---
title:    "Python: Merkkijonon muuntaminen isoin kirjaimin"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Joskus haluat muuttaa merkkijonon ensimmäisen kirjaimen isoksi. Tähän voi olla monia erilaisia syitä, kuten yhdenmukaistaminen tai näyttävämpi ulkoasu.

## Miten

```Python
# Luo merkkijono
string = "tämä on esimerkki"

# Käytä capitalize-funktiota muuttaaksesi ensimmäisen kirjaimen isoksi
capitalized_string = string.capitalize()

# Tulostaa: Tämä on esimerkki
print(capitalized_string)
```

## Syvempi sukellus

`capitalize()`-funktio muuttaa merkkijonon ensimmäisen kirjaimen isoksi ja kaikki muut kirjaimet pieniksi. Tämä funktio on hyödyllinen myös silloin, kun haluat muuttaa useamman kuin yhden kirjaimen isoksi, mutta ensimmäinen kirjain on ainoa, joka muutetaan automaattisesti isoksi.

## Katso myös

- [Official Python String Methods Documentation](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- [Python String Methods Tutorial](https://www.w3schools.com/python/python_strings_methods.asp)