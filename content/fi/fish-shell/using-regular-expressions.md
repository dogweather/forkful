---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Haskell: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Säännölliset lausekkeet tai "regex:it" ovat tehokas tapa vastata kysymyksiin kuin "löytyykö tästä merkkijonosta tietty kuvio?" Ohjelmoijat käyttävät regex:ja tekstien käsittelyssä, kuten syötteen validoinnissa, tiedostojen jäsennyksessä ja datan suodattamisessa.

## Miten:
```Fish Shell
# Luoja funktio joka luetella kaikki teksti tiedostot päätteellä .txt
function list_txt_files
   for file in *.txt
       echo $file
   end
end

# Suorita funktio
list_txt_files
```
Tässä esimerkissämme Fish Shell tulostaa kaikki .txt-päätteiset tiedostot hakemistossa.

## Syvällisemmin:
Regex:ien historia juontaa juurensa 1950-luvun matemaattisiin malleihin, joita sovellettiin 1970-luvulla Unix-tekstieditoriin. Regex:eille on muitakin vaihtoehtoja, kuten jäsennyspuut ja synteettiset ilmaisimet. Fish Shell toteuttaa regex:it POSIX-yhteensopivalla tavalla, ja se tarkistaa vastaavuudet laiskasti, mikä tarkoittaa, että se lopettaa tarkistuksen heti kun match löytyy.

## Katso myös:
1. [Fish Shellin viralliset dokumentit](https://fishshell.com/docs/current/index.html)
2. [POSIXin määrittelemät säännölliset lausekkeet](https://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap09.html)
3. [Unixin ed:in manuaali](https://www.gnu.org/software/ed/manual/ed_manual.html#Regular-Expressions), jossa regex:it ensin esiteltiin.