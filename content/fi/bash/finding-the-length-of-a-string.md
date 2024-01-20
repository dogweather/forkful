---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonon pituuden löytäminen tarkoittaa sitä, kuinka monta merkkiä merkkijonossa on. Ohjelmoijat tarvitsevat tätä yleisesti tiedon käsittelyyn tai tietojen vertailuun.

## Kuinka Tehdään:

Alla on esimerkkikoodi merkkijonon pituuden löytämiseen Bashissa:

```Bash
#!/bin/bash
myString="Hei Suomi"
lengthOfMyString=${#myString}
echo $lengthOfMyString
```

Kun ajat yllä olevan koodin, se tulostaa "9", mikä on merkkijonon "Hei Suomi" pituus.

## Syvällisemmin

Bashin kyky löytää merkkijonon pituus on ollut olemassa pitkään, ja se on osa shell-ohjelmoinnin perustietoa. 

Vaihtoehtoisesti, voit käyttää 'expr' tai 'awk' komentoa merkkijonon pituuden määrittämiseen. Kuitenkin, ${#string} syntaksi on suoraviivaisin ja tehokkain tapa tässä tilanteessa.

Bash käyttää sisäisesti C-kieltä merkkijonon pituuden määrittämiseen. Tämä tekee siitä nopean ja tehokkaan myös suurten merkkijonojen käsittelyssä.

## Katso Myös

Jos haluat tutkia lisää Bashista ja merkkijonon käsittelystä, alla on joitakin hyödyllisiä linkkejä:

1. [Bash beginners guide](http://tldp.org/LDP/Bash-Beginners-Guide/html/index.html)

Loppujen lopuksi, Bashin merkkijonon pituuden määrittäminen on hyödyllinen taito, joka helpottaa tietojen käsittelyä.+