---
title:                "JSON-tiedostojen käsittely"
date:                  2024-01-19
simple_title:         "JSON-tiedostojen käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Mikä ja Miksi?
JSON on kevyt dataformaatti, jota käytetään tietojen tallentamiseen ja verkon yli siirtämiseen. Ohjelmoijat käsittelevät JSON-tiedostoja, koska ne ovat helppolukuisia ihmisille ja helppo jäsentää koneille.

## Kuinka:
```Bash
# JSON-tiedoston purkaminen jq:lla
cat data.json | jq '.'

# Tietyn avaimen arvon hakeminen
jq '.key' data.json

# Uuden JSON-tiedoston luominen komennolla
echo '{"name": "Linux", "type": "Operating System"}' > os.json
```

Esimerkkituloste JSON-tiedoston purkamisesta jq:lla:
```Bash
{
  "name": "Linux",
  "type": "Operating System"
}
```

## Syväsukellus:
JSON, lyhenne sanoista JavaScript Object Notation, on syntynyt 2000-luvun alussa web-kehityksen tarpeisiin. Vaihtoehtoina käytetään esimerkiksi XML:tä, mutta JSON voittaa usein selkeydellään ja lyhyemmillä viesteillään. JSONin käsittelyssä käytetään yleensä valmiita kirjastoja, kuten `jq` komentorivillä tai vastaavia työkaluja eri ohjelmointikielissä.

## Näin Lisäksi:
- `jq`-työkalun ohjeet ja dokumentaatio: [stedolan.github.io/jq](https://stedolan.github.io/jq/)
- Bash-skriptaaminen: [gnu.org](https://www.gnu.org/software/bash/manual/bash.html)
