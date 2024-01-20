---
title:                "JSON-tiedostojen käsittely"
html_title:           "Arduino: JSON-tiedostojen käsittely"
simple_title:         "JSON-tiedostojen käsittely"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Työskenteleminen JSON:n kanssa tarkoittaa JavaScript Object Notation -muotoisten tietojen käsittelyä. Ohjelmoijat käyttävät JSON:ia sen yksinkertaisuuden ja web-teknologioiden yhteensopivuuden takia.

## How to: (Kuinka tehdä:)
Fish Shell:ssä JSON-tiedon käsittely onnistuu `jq`-työkalulla. Asenna `jq` ja käsittele JSON-tietoa näin:

```Fish Shell
# Asenna jq
sudo apt install jq

# Tallenna JSON-tiedostoon
echo '{"nimi": "Mikko", "ammatti": "Ohjelmoija"}' > henkilo.json

# Tulosta kenttä jq:lla
cat henkilo.json | jq '.nimi'
```

Esimerkin tuloste:
```
"Mikko"
```

## Deep Dive (Syväsukellus)
JSON julkaistiin 2000-luvun alussa helpottamaan tietojen siirtoa. XML oli ennen JSON:ia valtavirtaa datan vaihtoon, mutta JSON vei voiton yksinkertaisuudellaan ja luettavuudellaan. Fish Shell:ssä `jq` on yksi tehokkaimmista työkaluista JSON:n käsittelyyn, mutta vaihtoehtoisesti voi käyttää myös Python- tai Node.js-skriptejä.

## See Also (Katso Myös)
- jq:n viralliset dokumentit: https://stedolan.github.io/jq/manual/
- Fish Shell:n kotisivu: https://fishshell.com/
- JSON:n virallinen määrittely: https://www.json.org/json-fi.html