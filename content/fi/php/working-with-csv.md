---
title:                "CSV-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"

category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Työskentely CSV:n kanssa - "mikä ja miksi?" CSV on Comma-Separated Values tiedostomuoto datan tallentamiseen. Ohjelmoijat käyttävät sitä helpon siirrettävyyden ja ihmisen lukuisten ominaisuuksien vuoksi.

## How to:
Lue CSV-tiedosto:
```php
<?php
$filename = 'data.csv';
if (($handle = fopen($filename, "r")) !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        print_r($data);
    }
    fclose($handle);
}
?>
```
Luo CSV-tiedosto:
```php
<?php
$list = array(
    array('otsikko1', 'otsikko2', 'otsikko3'),
    array('data1', 'data2', 'data3'),
    array('data4', 'data5', 'data6')
);

$fp = fopen('file.csv', 'w');

foreach ($list as $fields) {
    fputcsv($fp, $fields);
}

fclose($fp);
?>
```
## Deep Dive
CSV on vanhaa teknologiaa, mutta edelleen laajasti käytössä joustavuutensa vuoksi. Vaihtoehtoina ovat esimerkiksi JSON ja XML, mutta ne eivät välttämättä ole yhtä yksinkertaisia ​​parannetun informaation hierarkian vuoksi. PHP:n `fgetcsv` ja `fputcsv` funktiot helpottavat CSV-tiedostojen käsittelyä ja ovat olleet kiinteä osa kieltä version 4.x asti.

## See Also
- CSV-tiedoston muotoilun standardi: [RFC 4180](https://tools.ietf.org/html/rfc4180)
- Vaihtoehdot CSV:lle: [JSON](https://www.json.org/json-en.html) ja [XML](https://www.w3.org/XML/)
