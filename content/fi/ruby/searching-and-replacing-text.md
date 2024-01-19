---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

### Mikä & Miksi?
Tekstin hakeminen ja korvaaminen on toiminto, jossa ohjelma etsii tietämme merkkijonon ja korvaa sen toisella. Ohjelmoijat käyttävät tätä ominaisuutta esimerkiksi virheiden korjaamiseen tai tiedon päivittämiseen ohjelmakoodeissa.

## Miten se tehdään:
Rubyssa voimme käyttää `gsub`-metodia tekstin hakemiseen ja korvaamiseen. 
```Ruby
teksti = "Tervetuloa Helsingin satamaan."
korvattu_teksti = teksti.gsub("satamaan", "korvaava_sana")
puts korvattu_teksti
```
Tulostettu tieto:
```Ruby
"Tervetuloa Helsingin korvaava_sana."
```
## Syvempi sukellus: 
Tekstin hakeminen ja korvaaminen on ollut osa ohjelmointikieliä jo kauan, tarjoten yksinkertaisen tavan muuttaa tekstiä. Rubyssä, `gsub`-metodin lisäksi, on myös muita menetelmiä, kuten `tr` ja `tr!`. Jokaisella näistä metodeista on omat etunsa ja haittansa, ja ohjelmoijan on valittava sopivin menetelmä kuhunkin tilanteeseen. `gsub` on yleisin ja monipuolisin menetelmä, joka soveltuu useimpiin tilanteisiin.

## Katso myös: 
Jos haluat syventää tietämystäsi Rubyn tekstin hakemisesta ja korvaamisesta, tässä on joitakin linkkejä, joista voi olla hyötyä:
- [Rubyn virallinen dokumentaatio](https://ruby-doc.org/core-3.0.0/String.html#method-i-gsub)
- [Stack Overflow](https://stackoverflow.com/questions/19445003/using-ruby-gsub-to-replace-a-string-from-an-array-of-strings)