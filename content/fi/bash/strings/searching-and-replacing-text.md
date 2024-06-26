---
date: 2024-01-20 17:57:30.534575-07:00
description: "How to: (Kuinka tehd\xE4:) Esimerkki tuloste."
lastmod: '2024-04-05T21:53:58.297368-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) Esimerkki tuloste."
title: Tekstin etsiminen ja korvaaminen
weight: 10
---

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
