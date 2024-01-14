---
title:    "PHP: Kahden päivämäärän vertailu"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla kahta päivämäärää?

Vertailemalla kahta päivämäärää, voit selvittää, kumpi päivämäärä on aikaisempi tai myöhempi. Tämä voi olla hyödyllistä esimerkiksi tapahtumien järjestämisessä tai tietyn ajanjakson laskemisessa.

## Miten vertailla kahta päivämäärää?

Käytä PHP: n sisäänrakennettua "strtotime" -toimintoa muuntaaksesi päivämäärät Unix-ajaksi ja vertaile sitten niitä käyttämällä "if" -lauseketta.

```PHP
$date1 = strtotime("2020-01-01");
$date2 = strtotime("2020-02-01");

if ($date1 < $date2) {
  echo "Ensimmäinen päivämäärä on aikaisempi kuin toinen.";
} else {
  echo "Toinen päivämäärä on aikaisempi kuin ensimmäinen.";
}
```

Tämä koodi tulostaisi "Ensimmäinen päivämäärä on aikaisempi kuin toinen."

## Syventävä sukellus

Päivämäärän vertailuun voi liittyä monia eri tekijöitä, kuten aikavyöhykkeet ja kesäaika. On tärkeää varmistaa, että päivämäärät on muunnettu samassa aikavyöhykkeessä vertailua varten.

Voit myös käyttää erityistä "diff" -toimintoa, joka laskee päivien, kuukausien ja vuosien erot kahden päivämäärän välillä.

``PHP
$date1 = strtotime("2020-01-01");
$date2 = strtotime("2020-02-01");

$diff = date_diff($date1, $date2);

echo "Ero on " . $diff->format("%m") . " kuukautta.";
```

Tämä koodi tulostaisi "Ero on 1 kuukausi."

## Katso myös

- [PHP Manuaali - strtotime](https://www.php.net/manual/en/function.strtotime.php)
- [PHP Manuaali - if statement](https://www.php.net/manual/en/control-structures.if.php)
- [PHP Manuaali - date_diff](https://www.php.net/manual/en/function.date-diff.php)