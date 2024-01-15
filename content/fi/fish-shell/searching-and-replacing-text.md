---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Fish Shell: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Miksi

Miksi joku haluaisi etsiä ja korvata tekstiä? Hyvä kysymys! Monissa tilanteissa saattaa olla tarpeellista tehdä suuria muutoksia tekstissä, olipa se sitten korjausvirhe tai tiettyjen sanojen vaihtaminen. Näin säästät aikaa ja vaivaa muutosten tekemisessä käsin.

# Miten

Fish Shell tarjoaa helpon ja tehokkaan tavan etsiä ja korvata tekstiä. Voit käyttää `sed` komentoa tai `string replace` toimintoa, ja molemmat toimivat samalla tavalla. Katso alla olevia esimerkkejä ja tuloskuvakaappauksia nähdäksesi, kuinka helppoa se on.

```Fish Shell
# Käytä "sed" komentoa etsiäksesi ja korvataksesi tekstiä
sed 's/vanha/uusi/g' tiedostonimi

# Käytä "string replace" funktiota korvataksesi tekstiä tietyn merkkijonon kanssa
string replace "vanha" "uusi" tiedostonimi
```

Tässä ensimmäisessä esimerkissä käytetään `sed` komentoa korvaamaan kaikki "vanha" esiintymät tiedostossa "uusi" sanalla. Toisessa esimerkissä käytetään `string replace` funktiota korvaamaan vain ensimmäinen esiintymä "vanha" sanalla "uusi". Tuloksena molemmissa tapauksissa on sama muokattu tiedosto.

# Syvemmälle

Fish Shellin `sed` komento ja `string replace` funktio perustuvat samaan ideaan: löytää tietty merkkijono ja korvata se toisella. Voit myös käyttää "s/abc/def/" muotoa antaaksesi tarkan vaihtamisen vain tiettyyn osaan tiedostoa.

HUOM: Jos haluat tehdä muutoksia pysyvästi tiedostoon, käytä `-i` vaihtoehtoa `sed` komennossa.

# Katso myös

- [Fish Shell dokumentaatio](https://fishshell.com/docs/current/)
- [Sed komento opetusohjelma](https://www.grymoire.com/Unix/Sed.html)
- [String Replace funktio opetusohjelma](https://commandcenter.blogspot.com/2014/01/command-of-week-string-replace-utility.html)