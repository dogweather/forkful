---
title:                "Bash: Työskentely json:n kanssa"
simple_title:         "Työskentely json:n kanssa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi käyttää JSONia Bash-ohjelmoinnissa?

JSON (JavaScript Object Notation) on yksi yleisimmistä tiedonformaateista, jota käytetään tiedonsiirrossa. Se on erityisen suosittu web-kehityksessä, mutta sen käyttömahdollisuudet laajenevat myös Bash-ohjelmoinnin maailmaan. JSONia käyttäen voit helposti lukea ja kirjoittaa tietoja eri muodoissa oleviin tiedostoihin, kuten CSV- tai XML-tiedostoihin.

## Miten käyttää JSONia Bash-ohjelmoinnissa?

JSON-tietojen käsittely Bashilla on helppoa ja nopeaa. Seuraavassa esimerkissä näytämme, kuinka luodaan yksinkertainen JSON-tiedosto ja lukea siitä tietoja Bash-skriptissä.

```
# Luodaan uusi JSON-tiedosto
cat > tiedosto.json <<EOF
{
  "nimi": "Matti",
  "ikä": 35,
  "ammatti": "Ohjelmoija"
}
EOF

# Luetaan JSON-tiedostosta tietoja ja tallennetaan muuttujiin
nimi=$(cat tiedosto.json | jq -r '.nimi')
ika=$(cat tiedosto.json | jq -r '.ikä')
ammatti=$(cat tiedosto.json | jq -r '.ammatti')

# Tulostetaan muuttujien arvot
echo "Tervehdys, nimeni on $nimi. Olen $ika-vuotias ja työskentelen ohjelmoijana."

# Tulostaa:
# Tervehdys, nimeni on Matti. Olen 35-vuotias ja työskentelen ohjelmoijana.
```
JSON-tiedostojen käsittelyyn Bashilla käytetään yleisesti [jq](https://stedolan.github.io/jq/) -työkalua. Sen avulla voidaan käsitellä ja muokata JSON-muotoista dataa helposti Bash-skripteissä. Käy tutustumassa jq:n dokumentaatioon saadaksesi lisätietoja sen käytöstä.

## Syväsukellus JSONin maailmaan

JSON-tiedoston sisältämän datan käsittelyllä on paljon mahdollisuuksia. Voit esimerkiksi muuttaa tiedoston sisältöä, lisätä uusia tietoja tai luoda uusia tiedostoja Bash-skripteillä. Voit myös hyödyntää muita työkaluja, kuten [curl](https://curl.se/), hakeaksesi JSON-dataa verkkosivuilta. Mahdollisuudet ovat lähes rajattomat ja vain mielikuvitus on rajana.

## Katso myös

- jq:n dokumentaatio: https://stedolan.github.io/jq/
- JSON:n perusteet: https://www.json.org/json-fi.html
- JSON-tietomuodon validointi Bashilla: https://github.com/alexbolboaca/valid-json/tree/master/bash