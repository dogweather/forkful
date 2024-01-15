---
title:                "Kahden päivämäärän vertailu"
html_title:           "PHP: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla kaksi päivämäärää?

Päivämäärät ovat tärkeitä tietoja monissa ohjelmointiprojekteissa ja vertailemalla kahta päivämäärää voimme tarkistaa niiden suhdetta ja laskea kuluneen ajan. Esimerkiksi vertailemalla nykyhetkeä ja tulevaa tapahtumaa voimme näyttää käyttäjälle, kuinka monta päivää tai tuntia on jäljellä.

## Miten vertailla kaksi päivämäärää?

Vertaileminen kahden päivämäärän välillä on helppoa PHP:llä. Käytämme ensin `strtotime()`-funktiota muuttaaksemme päivämäärät Unix-aikaleimoiksi, jotka ovat sekunteja vuodesta 1970 lähtien. Sitten voimme käyttää `<` ja `>` operaattoreita vertailemaan aikaleimoja. Katso alla olevaa esimerkkiä:

```PHP
$nyt = strtotime("now"); // nykyhetken aikaleima
$tuleva_tapahtuma = strtotime("25 December 2021"); // tulevan tapahtuman aikaleima

if ($nyt < $tuleva_tapahtuma) {
  echo "Tuleva tapahtuma on vasta " . floor(($tuleva_tapahtuma - $nyt) / 86400) . " päivän päässä!";
}
```

Tämä koodi tulostaa "Tuleva tapahtuma on vasta 13 päivän päässä!", jos suoritat sen esimerkiksi joulukuun 12. päivä.

## Syvemmälle päivämäärien vertailuun

PHP tarjoaa myös muita hyödyllisiä funktioita päivämäärien vertailuun, kuten `date_diff()`, joka laskee kahden päivämäärän välisen eron haluamassasi muodossa, ja `date_interval_create_from_date_string()`, jolla voit luoda haluamasi aikavälin päivämääriin perustuen. Sekä `strtotime()` että `date()` voivat käsitellä myös päivämääriä muodossa "YYYY-MM-DD", joten voit helposti vertailla tarkkojakin päivämääriä.

## Katso myös

- [PHP:n virallinen dokumentaatio päivämääräfunktioista](https://www.php.net/manual/en/function.date.php)
- [PHP.net: Päivämäärien vertailu](https://www.php.net/manual/en/datetime.diff.php)