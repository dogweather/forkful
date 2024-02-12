---
title:                "Kahden päivämäärän vertailu"
aliases:
- /fi/php/comparing-two-dates/
date:                  2024-01-20T17:33:22.340326-07:00
model:                 gpt-4-1106-preview
simple_title:         "Kahden päivämäärän vertailu"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Vertailemme kahta päivämäärää, kun haluamme tietää niiden ajallisen eron tai selvittää kumpi on aikaisempi tai myöhäisempi. Tämä on tärkeää esimerkiksi vanhentuneiden tiedostojen karsimisessa tai tapahtumien ajoittamisessa.

## Kuinka:
```php
<?php
$date1 = new DateTime("2023-04-01");
$date2 = new DateTime("2023-04-05");

if ($date1 < $date2) {
    echo "Ensimmäinen päivämäärä on aikaisempi.\n";
} else {
    echo "Toinen päivämäärä on aikaisempi tai ne ovat samat.\n";
}

$diff = $date1->diff($date2);
echo "Ero on {$diff->days} päivää.\n";
?>
```
Tulostus:
```
Ensimmäinen päivämäärä on aikaisempi.
Ero on 4 päivää.
```

## Syväluotaus
Päivämäärien vertailu on ollut PHP:n valikoimassa jo vuosikymmeniä, mutta DateTime-luokka toi mukanaan monia parannuksia vanhoihin funktioihin. Valinnaisesti, funktioilla `strtotime()` ja `date_diff()` voidaan saavuttaa sama lopputulos. DateTime-luokka tarjoaa kuitenkin paremman virheenhallinnan ja on objektilähtöisen suunnittelun mukainen. Käytä mieluiten DateTime-objekteja, sillä ne ovat joustavampia ja selkeämpiä.

## Katso Myös
- PHP:n virallinen dokumentaatio päivämäärien käsittelystä: [PHP: Date/Time Functions](https://www.php.net/manual/en/book.datetime.php)
- DateTime-luokan käyttö: [PHP: DateTime](https://www.php.net/manual/en/class.datetime.php)
- Päivämääräerojen laskeminen: [PHP: DateInterval](https://www.php.net/manual/en/class.dateinterval.php)
