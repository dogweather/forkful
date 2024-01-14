---
title:                "PHP: Päivämäärän saaminen"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi
Miksi olisi hyödyllistä hankkia nykyinen päivämäärä?

## Miten
```PHP
$date = date('d.m.Y');
echo "Tänään on " . $date;
```
Tämä koodinpätkä hakee nykyisen päivämäärän ja tulostaa sen näytölle. Tuloste näyttäisi tältä: "Tänään on 12.10.2021". Voit myös muotoilla päivämäärän haluamallasi tavalla, kuten vaihtamalla "d.m.Y" toiseen päivämääränmuotoon.

## Syvällinen sukellus
Päivämäärän hakeminen on tärkeä osa PHP-ohjelmointia. Se auttaa sinua näyttämään ajankohtaista tietoa käyttäjillesi ja järjestelmälle. Voit myös käyttää monia muita funktioita, kuten "strtotime()" ja "mktime()", saadaksesi erilaisia päivämäärätulosteita.

## Katso myös
- [PHP:n virallinen dokumentaatio päivämäärän hakemisesta](https://www.php.net/manual/en/function.date.php)
- [Vinkkejä erilaisten päivämäärämuotojen käyttämiseen PHP:ssa](https://www.w3schools.com/php/php_date.asp)
- [Lyhyt opas PHP:n päivämääräfunktioiden käyttöön](https://www.php.net/manual/en/ref.datetime.php)