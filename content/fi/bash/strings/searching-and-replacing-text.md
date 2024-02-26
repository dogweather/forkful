---
date: 2024-01-20 17:57:30.534575-07:00
description: "Tekstin etsiminen ja korvaaminen on prosessi, jossa l\xF6yd\xE4t tietty\xE4\
  \ teksti\xE4 ja vaihdat sen toiseen. Ohjelmoijat k\xE4ytt\xE4v\xE4t sit\xE4 koodin\
  \ virheiden\u2026"
lastmod: '2024-02-25T18:49:53.635318-07:00'
model: gpt-4-1106-preview
summary: "Tekstin etsiminen ja korvaaminen on prosessi, jossa l\xF6yd\xE4t tietty\xE4\
  \ teksti\xE4 ja vaihdat sen toiseen. Ohjelmoijat k\xE4ytt\xE4v\xE4t sit\xE4 koodin\
  \ virheiden\u2026"
title: Tekstin etsiminen ja korvaaminen
---

{{< edit_this_page >}}

## What & Why? (Mikä ja Miksi?)
Tekstin etsiminen ja korvaaminen on prosessi, jossa löydät tiettyä tekstiä ja vaihdat sen toiseen. Ohjelmoijat käyttävät sitä koodin virheiden korjaamiseen, asetusten päivittämiseen ja tylsän toistotyön välttämiseen.

## How to: (Kuinka tehdä:)
```Bash
# Etsi ja korvaa kertakäyttöisesti tiedostossa käyttäen sed-komentoa
sed 's/vanha/uusi/g' tiedosto.txt

# In-place korvaaminen (tiedosto muuttuu)
sed -i 's/vanha/uusi/g' tiedosto.txt

# Etsi kaikki esiintymät hakemistosta rekursiivisesti ja korvaa ne käyttäen grep ja sed
grep -rl 'vanha' ./hakemisto/ | xargs sed -i 's/vanha/uusi/g'
```

Esimerkki tuloste:
```
$ echo "Hei maailma" | sed 's/maailma/world/'
Hei world
```

## Deep Dive (Syväsukellus)
Tekstin etsiminen ja korvaaminen on vanha konsepti, joka on ollut tietokoneohjelmoinnissa jo sen alkuaikoina. Se perustuu säännöllisiin lausekkeisiin (regular expressions, regex), jotka mahdollistavat monimutkaistenkin kuvioiden löytämisen teksteistä. Vaihtoehtoina ovat modernit työkalut kuten `ack`, `ag`, tai `rg` (ripgrep), jotka ovat nopeampia ja monipuolisempia. Sed-komennon ('stream editor') toteutus yksinkertaisille korvauksille on yksi Unix-pohjaisten järjestelmien vanhimpia työkaluja ja se käyttää suoraviivaista luku-, korvaus- ja tulostusprosessia.

## See Also (Katso Myös)
- GNU sed manual: https://www.gnu.org/software/sed/manual/sed.html
- Regular Expressions Info: https://www.regular-expressions.info/
- The Silver Searcher (`ag`): https://github.com/ggreer/the_silver_searcher
- ripgrep (`rg`): https://github.com/BurntSushi/ripgrep
