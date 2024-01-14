---
title:                "PHP: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla kahta päivämäärää?

Päivämäärien vertailu on yleinen tarve ohjelmoinnissa ja se voi olla hyödyllistä esimerkiksi tietokannoista tai verkkosovelluksista saatavien tietojen käsittelyssä. Vertailemalla kahta päivämäärää voit tarkistaa, onko jokin tapahtuma tai muutos tapahtunut tietyn ajanjakson sisällä.

## Kuinka tehdä se?

PHP:llä päivämäärien vertailu on helppoa ja nopeaa. Käytännössä tarvitset vain kaksi päivämäärää ja vertailuoperaattorin, kuten ">", "<" tai "==". Voit myös käyttää PHP:n valmiita funktioita, kuten "strtotime()". Esimerkiksi:

```PHP
$paivamaara1 = "2020-01-01";
$paivamaara2 = "2020-02-01";

if ($paivamaara1 > $paivamaara2) {
    echo "Päivämäärä 1 on myöhempi kuin päivämäärä 2.";
} else if ($paivamaara1 < $paivamaara2) {
    echo "Päivämäärä 2 on myöhempi kuin päivämäärä 1.";
} else {
    echo "Päivämäärät ovat samat.";
}

// Output: Päivämäärä 2 on myöhempi kuin päivämäärä 1.
```

## Syvempiä pohdintoja

Päivämäärien vertailu voi aiheuttaa haasteita, kun otetaan huomioon erilaiset aikavyöhykkeet ja kellonajat. Tässä tapauksessa on tärkeää varmistaa, että molemmat päivämäärät ovat samassa muodossa, jotta vertailu on tarkka. Lisäksi, jos vertaat päivämääriä tietokannan tietoihin, muista huomioida tietokannan käyttämä aikavyöhyke ja käyttää tarvittaessa muuntofunktioita.

## Katso myös

- [PHP:n virallinen dokumentaatio päivämäärien vertailusta](https://www.php.net/manual/en/datetime.formats.relative.php)
- [Artikkeli: "Date and Time in PHP", Codecademy](https://www.codecademy.com/articles/date-and-time-in-php)
- [Päivämäärien vertailun kurssimateriaali, Tietokantaohjelmointi-kurssi, Avoin yliopisto](https://sis-tunti.csc.fi/avoin/amkht2018/visualisoint/VisualisoidutLuennot/FS/PAA.php?txtThisCurCode=775061S&txtLanguage=&txtThisCurVerName=kurssi&pageSt=K&p=?lv=fs&RO=1&agsProgrammeCode=--+&optionKumo=true)