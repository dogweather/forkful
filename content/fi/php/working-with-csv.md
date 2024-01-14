---
title:                "PHP: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi työskennellä CSV-tiedostojen kanssa

CSV-tiedostot (comma-separated values) ovat yksi yleisimmistä tiedostomuodoista PHP-ohjelmoijille. Ne mahdollistavat helpon ja tehokkaan tavan tallentaa ja käsitellä suuria määriä tietoa. Tässä blogipostissa käymme läpi, miksi ja miten kannattaa käyttää CSV-tiedostoja PHP-ohjelmoinnissa.

## Kuinka käyttää CSV-tiedostoja PHP:n kanssa

Käyttäminen CSV-tiedostojen kanssa PHP:ssa on yksinkertaista ja suoraviivaista. Ensimmäiseksi täytyy avata tiedosto käyttäen fopen-funktiota ja määrittää haluttu tilapäinen tiedostonimi. Esimerkiksi:

```PHP
$csv = fopen('data.csv', 'r');
```

Tämän jälkeen voimme käyttää while-silmukkaa lukeaksemme tiedostosta rivejä. Jokainen rivi tallennetaan taulukkoon käyttäen fgetcsv-funktiota. Esimerkiksi, jos CSV-tiedostossamme on rivi "Tommi, 32, Helsinki", silmukka näyttäisi tältä:

```PHP
while (($row = fgetcsv($csv)) !== FALSE) {
    // Käsittele tiedoston riviä
    $name = $row[0];
    $age = $row[1];
    $city = $row[2];
}
```

Lopuksi, voimme sulkea tiedoston käyttäen fclose-funktiota:

```PHP
fclose($csv);
```

Tämän jälkeen voimme käsitellä luettua tietoa tavallisilla PHP-kielen toiminnoilla taulukon muodossa.

## Syväluotausta CSV-tiedostojen kanssa työskentelyyn

CSV-tiedostojen kanssa työskentely vaatii tarkkuutta, sillä ne saattavat sisältää virheitä tai tyhjiä arvoja. On hyvä olla tarkistuksia, esimerkiksi tyhjyyden tarkistaminen ennen datan käyttämistä, jotta koodimme ei kaadu virheisiin.

Lisäksi, CSV-tiedoston muokkaamiseen voidaan hyödyntää fgetcsv-funktion lisäparametreja, kuten määrittämällä erotinmerkki tai jättämällä otsikkorivin huomiotta. Näitä kannattaa tutkia, jos CSV-tiedostojen kanssa työskentely tulee olemaan säännöllistä.

## Katso myös

- [PHP:n virallinen dokumentaatio CSV-tiedoston käsittelystä](https://www.php.net/manual/en/function.fgetcsv.php)
- [Ohjelmointiopas CSV-tiedostojen käsittelyyn PHP:ssa](https://www.codeofaninja.com/2013/02/php-csv-file.html)
- [Lisätietoa CSV-tiedostojen syntaksista ja käytöstä](https://en.wikipedia.org/wiki/Comma-separated_values)