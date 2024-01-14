---
title:                "PHP: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi
Usein PHP-ohjelmoinnissa joudutaan käsittelemään päivämääriä. Näiden päivämäärien muuntaminen string-muotoon on yleinen tehtävä, ja tässä blogikirjoituksessa käymme läpi, miten tämä tehdään käytännössä.

## Kuinka
Päivämäärän muuntaminen stringiksi onnistuu helposti PHP:n date-funktion avulla. Alla on esimerkki, miten voit muuntaa tämän päivän päivämäärän string-muotoon:

```PHP
echo date("d.m.Y"); // Tulostaa esimerkiksi 14.04.2021
```

Jos haluat tulostaa myös kellonajan, se onnistuu seuraavasti:

```PHP
echo date("d.m.Y H:i"); // Tulostaa esimerkiksi 14.04.2021 15:30
```

Voit myös antaa date-funktiolle parametrina annetun päivämäärän ja muuntaa sen haluamaasi muotoon. Esimerkiksi:

```PHP
$date = "2021-04-14";
echo date("d.m.Y", strtotime($date)); // Tulostaa 14.04.2021
```

## Deep Dive
PHP:ssa päivämäärän muuntamiseen stringiksi on muitakin keinoja kuin date-funktio. Voit esimerkiksi käyttää DateTime-luokkaa, joka tarjoaa enemmän vaihtoehtoja päivämäärän käsittelyyn. Alla on esimerkki siitä, miten DateTime-luokan avulla voit muuttaa päivämäärän stringiksi ja lisätä siihen tiettyä formaattia:

```PHP
$date = new DateTime();
echo $date->format("d.m.Y"); // Tulostaa esimerkiksi 14.04.2021
```

DateTime-luokalla voit myös helposti muuttaa aikavyöhykettä ja käsitellä erilaisia aikaleimoja.

## Katso myös
- [PHP:n date-funktio](https://www.php.net/manual/en/function.date.php)
- [DateTime-luokka](https://www.php.net/manual/en/class.datetime.php)
- [Päivämäärän muuttaminen timestampiksi](https://www.php.net/manual/en/function.strtotime.php)
- [Aikavyöhykkeet PHP:ssa](https://www.php.net/manual/en/timezones.php)