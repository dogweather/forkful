---
title:    "Fish Shell: Merkkijonon kirjoittaminen isoilla kirjaimilla"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Miksi

Miksi ohjelmoijien tulisi käyttää Fish Shellia pystyakseen muuttamaan merkkijonon ensimmäisen kirjaimen isoksi?

## Miten

Fish Shell tarjoaa helpon tavan muuttaa merkkijonon ensimmäinen kirjain isoksi. Voit käyttää `capitalize` komentoa ja antaa sille haluamasi merkkijonon. Alla on muutama esimerkki koodinpätkistä ja niiden tulosteet.

```Fish Shell
capitalize "helsinki"
=> Helsinki

capitalize "espanja"
=> Espanja

capitalize "koodari"
=> Koodari
```

## Syventävä tieto

Kun käytät `capitalize` komentoa Fish Shellissä, se muuttaa vain ensimmäisen kirjaimen isoksi. Jos haluat muuttaa koko merkkijonon isoksi, voit käyttää `string toupper` komentoa.

```Fish Shell
string toupper "suomi"
=> SUOMI

string toupper "kaupunki"
=> KAUPUNKI

string toupper "ohjelmointi"
=> OHJELMOINTI
```

## Katso myös

- [Fish Shell dokumentaatio](https://fishshell.com/docs/current/index.html)
- [Fish Shell opetusohjelma (englanniksi)](https://fishshell.com/docs/current/tutorial.html)
- [Fish Shell GitHub repository](https://github.com/fish-shell/fish-shell)

Kiitos lukemisesta ja muista nyt iskeä ensimmäinen kirjain isoksi Fish Shellilläsi!