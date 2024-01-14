---
title:                "PHP: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi
 JSON on yksi nykypäivän suosituimmista tiedon tallennusmuodoista. Se on sekä helppokäyttöinen että monipuolinen, ja sitä voidaan käyttää lähes kaikissa ohjelmointikielissä. PHP:ssa JSON on erityisen suosittu sen käyttäjäystävällisen syntaksin ja tehokkaiden sisäänrakennettujen toimintojen ansiosta. 

## Kuinka
Käsitelläksesi JSON-dataa PHP:ssa, sinun tulisi seurata näitä yksinkertaisia vaiheita:
- Käytä `json_encode()`-funktiota muuttaaksesi PHP-muuttujat JSON-muotoon
- Käytä `json_decode()`-funktiota muuttaaksesi JSON-muuttujat takaisin PHP:n muotoon
- Voit myös käyttää `json_encode()`-funktiota palauttamaan JSON-data suoraan HTTP-pyynnön vastauksena

Alla on esimerkki siitä, kuinka voit käyttää JSON:ia PHP:ssa:

```PHP
// Muuttuja PHP-muodossa
$auto = array("merkki" => "Toyota", "malli" => "Corolla", "valmistusvuosi" => 2015);

// Muutetaan muuttuja JSON-muotoon
$json_auto = json_encode($auto);
echo $json_auto;
```

Tulos:
```JSON
{
    "merkki": "Toyota",
    "malli": "Corolla",
    "valmistusvuosi": 2015
}
```

## Syvempään

#### Kerros 1: JSON-muuttujien käyttö
JSON-muuttuja koostuu avain-arvo pareista, jotka vastaavat PHP:n assosiaatiotauluja. Voit käsitellä JSON-dataa PHP:ssa kuten mitä tahansa muuta assosiaatiotaulua: saat tietyn arvon avaimen avulla käyttämällä haku-operaattoria `[ ]`.

#### Kerros 2: Muuntaminen monimutkaisempiin rakenteisiin
JSON tukee monimutkaisempia rakenteita, kuten taulukoita ja sisäkkäisiä objekteja. Voit käsitellä näitä rakenteita PHP:n `foreach`-silmukan avulla tai edelleen käyttämällä haku-operaattoria.

#### Kerros 3: Poimiminen reaaliaikaisesti
JSON-dataa tarjotaan usein reaaliaikaisesti verkon kautta REST-rajapintojen kautta. PHP:llä voit käyttää `file_get_contents()`-funktiota tulkitsemaan JSON-dataa ja `json_decode()`-funktiota muuttamaan sen PHP:n muotoon.

## Katso myös
- [PHP:n viralliset JSON-toiminnot](https://www.php.net/manual/en/ref.json.php)
- [JSON:n virallinen sivusto](https://www.json.org/json-fi.html)
- [PHP-json-dotnet - JSON-tietojen pakkaaminen ja purkaminen ASP.NET Core-microservicejen avulla](http://sheycoder.com/06-20-2019/php-json-dotnet-pakkaaminen-purkaminen/)