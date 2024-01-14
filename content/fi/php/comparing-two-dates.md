---
title:    "PHP: Kahden päivämäärän vertailu"
keywords: ["PHP"]
---

{{< edit_this_page >}}

Miksi vertailla kahta päivämäärää?

Päivämäärien vertaileminen on tärkeää, kun haluat tarkistaa, ovatko kaksi päivämäärää sama, tai jos haluat laskea ajanjakson kahden päivämäärän välillä. Vertailemalla päivämääriä voit myös järjestää tapahtumia tai tehtäviä päivämäärän mukaan.

Kuinka vertailla kahta päivämäärää?

Vertaamalla kahta päivämäärää PHP:llä on helppo tapa saada selville, onko toinen päivämäärä myöhempi, aikaisempi vai sama kuin toinen. Voit vertailla päivämääriä käyttämällä PHP:n sisäänrakennettuja funktioita, kuten "strtotime" ja "date_diff".

```
// Luodaan kaksi eri päivämäärää
$date1 = strtotime("2020-10-15");
$date2 = strtotime("2020-10-20");

// Vertaillaan päivämääriä ja tulostetaan tulos
if ($date1 < $date2) {
  echo "Ensimmäinen päivämäärä on aiempi kuin toinen päivämäärä.";
} elseif ($date1 > $date2) {
  echo "Ensimmäinen päivämäärä on myöhempi kuin toinen päivämäärä.";
} else {
  echo "Päivämäärät ovat samat.";
}

// Tulostus: Ensimmäinen päivämäärä on aiempi kuin toinen päivämäärä.
```

Voit myös laskea ajanjakson kahden päivämäärän välillä käyttämällä "date_diff" funktiota.

```
// Luodaan kaksi eri päivämäärää
$date1 = date_create("2020-01-01");
$date2 = date_create("2020-01-10");

// Lasketaan päivien ero
$diff = date_diff($date1, $date2);

// Tulostetaan tulos
echo 'Ero päivissä: ' . $diff->days;

// Tulostus: Ero päivissä: 9
```

Syvällinen sukellus

Päivämäärien vertailu voi joskus olla monimutkaisempaa, etenkin jos haluat ottaa huomioon myös kellonajan. Lisäksi, jos päivämäärät on tallennettu tietokantaan, saatat joutua käsittelemään niitä eri muodoissa. On tärkeää ymmärtää eri päivämäärämuotoja ja niiden välisiä eroja.

Yksi yleisimmistä ongelmatilanteista päivämäärävertailun yhteydessä on aikavyöhykkeiden huomioiminen. Päivämäärät voivat olla eri aikavyöhykkeillä, mikä voi vaikuttaa vertailun tulokseen. Tässä tapauksessa on suositeltavaa tallentaa päivämäärät UTC-aikavyöhykkeelle ja muuttaa ne näytettäessä käyttäjän aikavyöhykkeelle.

Katso myös

- PHP:n virallinen dokumentaatio päivämäärien vertailusta (https://www.php.net/manual/en/datetime.diff.php)
- Opas päivämäärien tallentamiseen ja käsittelyyn tietokannassa (https://www.w3schools.com/php/php_mysql_date.asp)
- Selitys eri päivämäärämuodoista ja niiden käytöstä (https://www.php.net/manual/en/datetime.formats.php)

Katso myös

- Päivämäärien vertailu PHP:llä (https://www.php.net/manual/en/datetime.diff.php)
- Opas päivämäärien tallentamiseen ja käsittelyyn tietokannassa (https://www.w3schools.com/php/php_mysql_date.asp)
- Selitys eri päivämä