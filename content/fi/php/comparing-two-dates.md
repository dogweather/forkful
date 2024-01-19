---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärien vertailu tarkoittaa kahden päivämäärän suhteellisen järjestyksen selvittämistä. Ohjelmoijat tekevät tämän usein aikaperusteisten toimintojen, kuten tapahtumien järjestämisen tai aikarajojen noudattamisen, hallitsemiseksi.

## Näin se tehdään:

Päivämäärien vertailu PHP:ssa on helppoa. Käytetään DateTime-objekteja, jotka luodaan new DateTime -kutsulla, ja vertailu tapahtuu operaattorilla `>` tai `<`. 

```PHP
<?php
$pvm1 = new DateTime('2021-05-10');
$pvm2 = new DateTime('2021-03-20');

if ($pvm1 > $pvm2) {
    echo 'Päivämäärä 1 on myöhempi kuin Päivämäärä 2.';
} else {
    echo 'Päivämäärä 2 on myöhempi kuin Päivämäärä 1.';
}
?>
```

Tämä koodi tulostaa: "Päivämäärä 1 on myöhempi kuin Päivämäärä 2."

## Syvemmälle:

PHP:n DateTime-luokka introdukoitiin versiossa 5.2, joka julkaistiin vuonna 2006, tätä ennen päivämäärät vertailtiin muuntamalla ne aikaleimoiksi (timestamp) funktiolla `strtotime()`. PHP 7.1:n myötä voimme myös käyttää `spaceship`-operaattoria `<=>` päivämäärien vertailuun.

```PHP
<?php
$pvm1 = new DateTime('2021-05-10');
$pvm2 = new DateTime('2021-03-20');

echo $pvm1<=>$pvm2;
?>
```

Koodi palauttaa 1 jos Päivämäärä 1 on myöhempi, -1 jos Päivämäärä 2 on myöhempi ja 0 jos ne ovat samat.

## Katso myös:

- DateTime-luokka PHP-dokumentaatiossa: https://www.php.net/manual/en/class.datetime.php
- PHP:n Vertailuoperaattorit: https://www.php.net/manual/en/language.operators.comparison.php
- PHP:n Spaceship-operaattori: https://www.php.net/manual/en/migration71.new-features.php
- Aikaleimojen muunnos PHP:ssa: https://www.php.net/manual/en/function.strtotime.php