---
title:                "Python: Hakeminen ja tekstin korvaaminen"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi?

Tekstin etsiminen ja korvaaminen on tärkeä osa ohjelmoinnin prosessia, joka mahdollistaa nopean ja tarkan tietojen muokkaamisen. Se on erityisen hyödyllistä, kun käsitellään suuria tiedostomääriä tai halutaan tehdä samanlaisia muutoksia useille tiedostoille.

## Kuinka tehdä se?

Tekstin etsiminen ja korvaaminen Pythonilla on helppoa. Voit käyttää sisäänrakennettua `replace()` -funktiota, joka korvaa kaikki annetun merkkijonon esiintymät toisella merkkijonolla. Voit myös käyttää `regex` -moduulia, joka mahdollistaa monimutkaisempia hakuja ja korvauksia.

```
# Perus esimerkki
teksti = "Hei maailma!"
korvattu_teksti = teksti.replace("maailma", "universumi")
print(korvattu_teksti)
# Tulostaa: Hei universumi!

# Regex esimerkki
import re
teksti = "Tämä on esimerkki 123 numerosta"
korvattu_teksti = re.sub("\d+", "456", teksti)
print(korvattu_teksti)
# Tulostaa: Tämä on esimerkki 456 numerosta
```

## Syvällinen sukellus

Tekstin etsiminen ja korvaaminen ei ole vain yksinkertaista merkkijonojen vaihtamista. Voit käyttää säännöllisiä lausekkeita (`regex`) sisällyttämään muuttujia ja ehtoja hakuusi, jolloin voit tehdä monimutkaisempia muutoksia. Voit myös kehittää omaa logiikkaasi `replace()` -funktion käytössä, esimerkiksi tehdä erilaisia muutoksia eri osille tekstiä.

## Katso myös

- [Python`replace()` dokumentaatio](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Python regex opas](https://docs.python.org/3/howto/regex.html)
- [Regex testeri](https://regex101.com/) (hyödyllinen testaamaan ja harjoittelemaan säännöllisiä lausekkeita)