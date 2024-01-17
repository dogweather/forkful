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

Mikä ja miksi?

Vertaillessaan kahta päivämäärää ohjelmoijat tarkastelevat kahden päivämäärän eroa toisiinsa nähden. Tämä on usein tarpeellista esimerkiksi sovelluksissa, jotka käsittelevät aikaa ja tapahtumia.

Miten:

```PHP
<?php
$date1 = "2021-01-01";
$date2 = "2021-12-31";

// Vertaillaan päivämääriä ja tulostetaan tulos
if ($date1 < $date2) {
  echo "Päivämäärä 1 on ennen Päivämäärää 2";
} else if ($date1 == $date2) {
  echo "Päivämäärä 1 on sama kuin Päivämäärä 2";
} else {
  echo "Päivämäärä 1 on jälkeen Päivämäärän 2";
}
?>
```

```PHP
Tulostaa:
Päivämäärä 1 on ennen Päivämäärää 2
```

Syvällisempää tietoa:

(1) Vertailua päivämäärien välillä on tarvittu jo pitkään, sillä ohjelmistoissa on usein tarvetta käsitellä aikaa ja tapahtumia. (2) Lisäksi on olemassa muitakin tapoja vertailla päivämääriä, kuten käyttämällä aikaleimoja tai muuntamalla päivämäärät numeerisiksi arvoiksi. (3) Tarkempien päivämäärävertailujen toteuttaminen voi edellyttää erilaisten kirjastojen käyttöä tai omien funktioiden luomista.

Katso myös:

- [PHP:n virallinen dokumentaatio päivämäärien vertailusta](https://www.php.net/manual/en/language.operators.comparison.php)
- [Ohjeita päivämäärien käsittelyyn PHP:ssä](https://www.w3schools.com/php/php_date.asp)