---
title:                "JSON-tiedostojen käsittely"
html_title:           "Arduino: JSON-tiedostojen käsittely"
simple_title:         "JSON-tiedostojen käsittely"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
JSON, JavaScript Object Notation, on dataformaatii, jonka avulla tiedot vaihtuvat siististi ja tiiviisti. Käytämme sitä koska se on helppolukuinen ihmisten ja koneiden kannalta ja koska se toimii saumattomasti lähes kaikkien ohjelmointikielien kanssa.

## How to: - Kuinka:
PHP:ssä JSON:ia käsitellään `json_encode` ja `json_decode` funktioilla. Tässä pari esimerkkiä:

```PHP
<?php
// Luodaan PHP-array
$data = array('nimi' => 'Jari', 'ammatti' => 'Koodari');

// Muunnetaan PHP-array JSON-stringiksi
$jsonData = json_encode($data);
echo $jsonData;
// Output: {"nimi":"Jari","ammatti":"Koodari"}

// Muunnetaan JSON-string takaisin PHP-arrayksi
$decodedData = json_decode($jsonData, true);
print_r($decodedData);
// Output: Array ( [nimi] => Jari [ammatti] => Koodari )
?>
```

JSON-stringistä tulee PHP-olio, jos `json_decode` funktiolle ei anneta toista parametriä (true).

## Deep Dive - Syväsukellus:
JSON muoto syntyi 2000-luvun alussa, helpottamaan selaimen ja palvelimen välistä kommunikointia. XML oli aiempi suosikki, mutta sen raskas luonne antoi tien JSONille. PHP käsittelee JSON-dataa nativiisti `json_encode` ja `json_decode` funktioiden kautta, jotka tulivat mukaan versiossa 5.2.0. JSON formaatti on tehokas, koska se on yksinkertainen, kevyt ja kielet välinen.

## See Also - Katso Myös:
- PHP:n virallinen JSON käsittelyn dokumentaatio: [php.net/manual/en/book.json.php](https://www.php.net/manual/en/book.json.php)
- JSON-syntaksin virallinen käsittely: [json.org](http://json.org/)
- Web-API:tietoa ja JSONin käyttö esimerkit: [Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON)
