---
title:                "Työskentely jsonin kanssa"
html_title:           "PHP: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/working-with-json.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Ohjelmoijat käyttävät JSON-tiedostoja muodostaakseen ja lukeakseen tietoja web-sovellusten välillä. JSON eli JavaScript Object Notation on kevyt tiedostomuoto, joka on helppo lukea ja kirjoittaa sekä ihmisille että ohjelmille.

## Näin teet sen:
Voit koodata JSON-tiedostoja käyttäen PHP:n integroitua json-funktiota. Tässä esimerkki:
```
$json = '{"nimi":"Matti","ikä":35,"osoite":"Esimerkkitie 5"}';
$data = json_decode($json);
echo $data->nimi; // tulostaa "Matti"
```

## Syvällinen selitys:
JSON kehitettiin JavaScriptin yhteydessä, mutta siitä on tullut laajalti käytetty formaatti myös muissa ohjelmointikielissä, kuten PHP:ssa. JSON on nopea ja kevyt, mikä tekee siitä optimaalisen valinnan web-sovellusten väliseen datan siirtoon. Vaihtoehtoisesti voit käyttää myös XML-formaattia, mutta se on usein monimutkaisempi ja vaatii enemmän resursseja.

## Katso myös:
Mikä on JSON: https://www.json.org/json-fi.html
PHP:n json-funktiot: https://www.php.net/manual/en/book.json.php