---
title:                "Työskentely jsonin kanssa"
html_title:           "C#: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
JSON on lyhenne sanoista JavaScript Object Notation ja se on yksi yleisimmistä tavoista tallentaa ja siirtää tietoa ohjelmoinnissa. Monet ohjelmoijat käyttävät JSONia, koska se on helppo ymmärtää ja käyttää, ja se on myös yhteensopiva monien ohjelmointikielten kanssa.

## Näin teet sen:
Ohjelmointiesimerkit ja esimerkkilähtö ```C# ...``` koodipalikoissa.

```C#
// Luodaan JSON-objekti
var json = "{ "name": "Maija", "age": 25, "hobby": "programming" }";

// Muunnamme JSON-objektin C#-objektiksi
var csharpObj = JsonConvert.DeserializeObject(json);

// Haetaan haluttu tieto C#-objektista
var name = csharpObj.name;

// Tulostetaan tieto konsoliin
Console.WriteLine($"Nimeni on {name}.");

```

Tulostus: Nimeni on Maija.

## Syväsukellus:
JSON kehitettiin alun perin JavaScriptin yhteyteen, mutta siitä on tullut hyvin suosittu tietojen tallennusmuoto myös muiden ohjelmointikielten, kuten C#, kanssa. JSONia käytetään usein web-sovellusten tiedonsiirrossa ja se on myös yhteensopiva monien tietokantojen kanssa. Joitakin vaihtoehtoisia tapoja tallentaa ja siirtää tietoa ovat esimerkiksi XML ja CSV. JSON on yleensä näistä vaihtoehdoista kevyempi ja helppokäyttöisempi.

## Katso myös:
- Newtonsoft.Json-kirjasto: https://www.newtonsoft.com/json
- JSON-opas: https://www.json.org/json-fi.html
- CSV vs. JSON: https://www.sitepoint.com/comparing-javascript-object-notation-json-real-children-comma-delimited-csv-format/