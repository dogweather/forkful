---
title:    "PHP: Päivämäärän hakeminen"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Miksi

Monet PHP-ohjelmoijat tietävät, että nykyisen päivämäärän saaminen on helppo tehtävä. Mutta miksi haluat tehdä tämän? On olemassa useita syitä, miksi saatat tarvita nykyistä päivämäärää ohjelmassasi. Yksi yleisimmistä on esimerkiksi kun haluat tallentaa käyttäjän viimeisimmän toiminnan päivämäärän tietokantaan. Tämä voi auttaa sinua seuraamaan käyttäjän toimintaa ja parantamaan sovelluksesi toiminnallisuutta.

## Miten

PHP:llä on valmiina funktio, joka palauttaa nykyisen päivämäärän ja ajan. Tämä funktio on nimeltään "date()" ja sen avulla voit helposti näyttää haluamasi päivämäärän ja ajan muodossa. Tässä on esimerkki:

```PHP
$date = date('d.m.Y H:i:s');
echo $date;
```
Tämä koodi tulostaa nykyisen päivämäärän ja ajan muodossa "päivä.kuukausi.vuosi tunti:minuutti:sekunti". Voit myös muuttaa muotoa antamalla "date()" -funktiolle eri parametreja. Esimerkiksi jos haluat näyttää vain päivämäärän, voit käyttää tätä koodia:

```PHP
$date = date('d.m.Y');
echo $date;
```

## Syvempi sukellus

"Date()" -funktion lisäksi PHP tarjoaa myös muita hyödyllisiä päivämäärän ja ajan manipulointiin tarkoitettuja funktioita, kuten "strtotime()" ja "DateTime". Voit myös helposti muuttaa päivämäärän muotoja käyttämällä "date_format()" -funktiota tai lisätä tai vähentää päiviä käyttämällä "date_add()" ja "date_sub()" -funktioita.

On myös tärkeää huomata, että nykyinen päivämäärä ja aika perustuu palvelimen aikaan. Jos haluat näyttää käyttäjälle tietyn aikavyöhykkeen mukaisen päivämäärän ja ajan, voit käyttää "date_default_timezone_set()" -funktiota.

## Katso myös

- [PHP:n date()-funktion dokumentaatio](https://www.php.net/manual/en/function.date.php)
- [PHP:n aika- ja päivämäärätoiminnot](https://www.php.net/manual/en/book.datetime.php)
- [Date and Time - PHP The Right Way](https://phptherightway.com/#date_and_time)