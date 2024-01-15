---
title:                "Tiedoston lukeminen"
html_title:           "PHP: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedoston lukeminen on olennainen osa PHP-ohjelmointia, sillä usein haluamme käsitellä ja tallentaa tekstimuotoisia tietoja. Tässä artikkelissa näytämme, miten voit lukea tekstitiedostoja PHP:lla ja miten voit hyödyntää tätä taitoa omassa ohjelmointityössäsi.

## Miten tehdä se

Lukeminen tekstitiedosto PHP:lla on helppoa käyttäen file_get_contents()-funktiota. Funktio ottaa parametrinaan tiedoston nimen ja palauttaa tiedoston sisällön merkkijonona. Voit tallentaa palautetun merkkijonon muuttujaan ja käyttää sitä edelleen. Katso alla oleva esimerkki:

```PHP
$tiedosto = file_get_contents("tiedostonimi.txt");
echo $tiedosto;
```

Tällä koodilla voit lukea tiedoston ja tulostaa sen sisällön näytölle. Voit myös käyttää muita PHP:n tiedostonkäsittelyfunktioita, kuten fopen() ja fread(), tarkempien lukemismahdollisuuksien saavuttamiseksi.

## Syventävää tietoa

Tiedoston lukeminen PHP:lla onnistuu myös eri tiedostomuodoissa, kuten CSV ja JSON. Voit käyttää PHP:n sisäänrakennettuja funktioita, kuten fgetcsv() ja json_decode(), näiden tiedostotyyppien käsittelyyn. Lisäksi voit käyttää erilaisia hakutoimintoja tiedoston sisällön tarkasteluun.

## Katso myös

- PHP:n virallinen dokumentaatio tiedostonkäsittelyfunktioista: https://www.php.net/manual/en/ref.filesystem.php
- W3Schools opetusohjelma tiedoston lukemisesta PHP:lla: https://www.w3schools.com/php/php_file.asp
- PHP-käyttäjätuen keskusteluketju tiedoston lukemisesta: https://stackoverflow.com/questions/9222973/how-to-read-a-file-line-by-line-to-an-array-in-php