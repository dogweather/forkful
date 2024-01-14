---
title:    "PHP: Stringin kapitalisointi"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Miksi koodata? 

Miksi koodata, erityisesti PHP:tä? Yksinkertainen vastaus: koska ohjelmointi on tulevaisuuden taito ja PHP on yksi suosituimmista ja monipuolisimmista ohjelmointikielistä. PHP:llä voit luoda dynaamisia ja interaktiivisia verkkosivustoja ja sovelluksia, mikä tekee siitä erittäin hyödyllisen taidon oppia.

## Kuinka tehdä se?

PHP:n opetteleminen voi vaikuttaa aluksi pelottavalta, mutta onneksi yksinkertaiset tehtävät voivat auttaa sinua pääsemään alkuun. Yksi tärkeimmistä taidoista, joita PHP:llä voi oppia, on merkkijonojen käsittely. Tässä esimerkissä keskitymme erityisesti merkkijonon muuttamiseen isolla alkukirjaimella.

```
<?php
$merkkijono = "hei, olen php-koodari!";

//ensimmäinen tapa käyttää ucfirst-funktiota
echo "Tervetuloa " . ucfirst($merkkijono) . "!";

//toinen tapa käyttää strtoupper- ja substr-funktioita
echo "Tervetuloa " . strtoupper(substr($merkkijono, 0, 1)) . substr($merkkijono, 1) . "!";
?>
```

Koodi tulostaa seuraavan:

```
Tervetuloa hei, olen php-koodari!
Tervetuloa Hei, olen php-koodari!
```

## Syvemmälle aiheeseen

Nyt kun olet oppinut perusteet, on aika kääriä hihat ja mennä syvemmälle aiheeseen. Miksi merkkijonon käsittely on tarpeellista? Yksi yleisimmistä syistä on tietokannan tiedon tarkistaminen ja käsittely. Esimerkiksi käyttäjien syöttämissä tiedoissa voi olla epätarkkuuksia, kuten merkkijonon alku pienaakkosella. PHP:n avulla voit helposti korjata nämä epätarkkuudet ja varmistaa, että tietokantaan tallennetut tiedot ovat kaikki samassa muodossa.

Lisäksi, omien kirjoittamien funktioiden avulla voit muuttaa merkkijonon käsittelyä vieläkin monipuolisemmaksi. Esimerkiksi voit luoda oman funktion, joka muuttaa sanan ensimmäisen kirjaimen isoksi ainoastaan silloin, kun sana on yhden merkin mittainen.

```
//funktio, joka muuttaa sanan ensimmäisen kirjaimen isoksi vain yhden merkin pituisissa sanoissa
function eka_kirjain_isolla($merkkijono) {
    if(strlen($merkkijono) == 1) {
        return strtoupper($merkkijono);
    }
    else {
        return $merkkijono;
    }
}

$merkkijono = "php";

echo eka_kirjain_isolla($merkkijono); //tulostaa PHp
```

## Katso myös

- [PHP:n opas merkkijonojen käsittelyyn](https://www.php.net/manual/en/language.types.string.php)
- [Lisää PHP:n merkkijonon muokkausfunktioita](https://www.php.net/manual/en/ref.strings.php)