---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Python: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Muutamalla sanalla selitä, miksi joku haluaisi laskea merkkijonon pituuden käyttämällä Python-ohjelmointia.

## Miten

```python
string = "Hei maailma!" # määritellään merkkijono
length = len(string) # käytetään len() funktiota laskeaksemme merkkijonon pituuden
print("Merkkijonon pituus on:", length) # tulostetaan pituus

```

**Tulostaa:**
``` 
Merkkijonon pituus on: 12
```

## Syventyminen

Merkkijonon pituuden laskeminen on yksinkertainen, mutta tärkeä tehtävä useimmissa ohjelmoinnin tehtävissä. Pythonissa käytämme `len()` funktiota, joka palauttaa merkkijonon pituuden numerona. On tärkeää muistaa, että välilyönnit lasketaan myös merkkeinä ja vaikuttavat merkkijonon kokonaispituuteen.

## Katso myös

- [Python kielto-opas](https://www.python.org)
- [Len() dokumentaatio](https://docs.python.org/3/library/functions.html)
- [Merkkijonojen käsittely Pythonissa](https://realpython.com/python-strings/)