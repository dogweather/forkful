---
title:                "Työskentely jsonin kanssa"
html_title:           "Javascript: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/working-with-json.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?
JSON eli JavaScript Object Notation on yleisesti käytetty tapa tallentaa ja vaihtaa dataa eri ohjelmointikielten välillä. Se on helposti luettava ja kirjoitettava, ja sen avulla pystytään tallentamaan monimutkaisiakin tietorakenteita. Ohjelmoijat käyttävät JSONia esimerkiksi web-sovellusten kehittämisessä ja tietokantojen kanssa työskennellessä.

## Kuinka:
Käytä JSON-dataa ```Javascript ... ``` koodilohkoissa ja tallenna se muuttujiin. Voit kutsua JSON-objektin arvoja muuttujista käyttäen pistenotaatiota. Esimerkiksi:

```Javascript
let henkilö = {
  nimi: "Matti Meikäläinen",
  ikä: 25,
  harrastukset: ["urheilu", "kirjoittaminen", "musiikin kuuntelu"]
};

console.log(henkilö.nimi); //tulostaa "Matti Meikäläinen"
```

## Syvemmälle:
JSON kehitettiin alun perin JavaScriptin osaksi, ja se on tullut erittäin suosituksi kieli- ja teknologia-independent tapa tallentaa dataa. Yksi sen suurista eduista on sen helppokäyttöisyys ja luettavuus ihmisen silmin. Vaikka JSON onkin yleisesti käytetty, on olemassa myös muita vaihtoehtoja dataformaateiksi, kuten XML ja CSV. Voit myös käyttää biblioteekkeja, kuten JSON.parse ja JSON.stringify, auttamaan JSON-datan käsittelyssä.

## Katso myös:
Voit lukea lisätietoja JSONista ja sen käytöstä täältä: [Mozilla Developer Network](https://developer.mozilla.org/fi/docs/Web/JavaScript/Reference/Global_Objects/JSON). Voit myös tutustua erilaisiin JSON lähdekoodi-kieliversioihin ja spesifikaatioihin täältä: [JSON.org](https://json.org/json-fi.html).