---
title:    "PHP: Säännöllisten lausekkeiden käyttö"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita ohjelmoinnissa?

Säännölliset lausekkeet ovat hyödyllinen työkalu ohjelmoijille, sillä ne mahdollistavat monimutkaisten tekstien etsimisen ja muokkaamisen nopeasti ja tehokkaasti. Ne ovat erityisen hyödyllisiä silloin, kun on tarve käsitellä erilaisia merkkijonoja ja tietueita, kuten esimerkiksi lomakkeiden syötteitä, sähköpostiosoitteita tai tiedostojen nimiä.

## Kuinka käyttää säännöllisiä lausekkeita PHP:ssa?

Säännöllisiä lausekkeita käytetään PHP:ssa preg_match-funktion avulla. Tämä funktio ottaa kolme parametria: säännöllinen lauseke, haettava merkkijono ja muuttuja, johon haun tulos tallennetaan. Käyttämällä erilaisia säännöllisiä lausekkeita, voit tarkistaa, vastaako annettu merkkijono haluttua kaavaa ja tehdä sen perusteella jotain muuta.

```
<?php 
$merkkijono = "Tämä on esimerkkimerkkijono";

// Tarkistetaan, sisältääkö merkkijono sanan "esimerkki"
if (preg_match("/esimerkki/", $merkkijono)) {
  echo "Merkkijono sisältää sanan 'esimerkki'";
} else {
  echo "Merkkijono ei sisällä sanaa 'esimerkki'";
}
?>
```

Tämä koodinpätkä tulostaisi "Merkkijono sisältää sanan 'esimerkki'".

## Syvempi sukellus säännöllisiin lausekkeisiin

Säännöllisiä lausekkeita voidaan käyttää moniin eri tarkoituksiin, kuten esimerkiksi hakemaan tiettyjä merkkijonoja, korvaamaan osia merkkijonosta tai tarkistamaan merkkijonojen muotoja. Niiden avulla voidaan myös suorittaa haun eri vaihtoehdoilla tai sallia tiettyjä sääntöjä, kuten esimerkiksi kirjainkoolla tai numeroiden läsnäololla.

Säännölliset lausekkeet voivat aluksi vaikuttaa monimutkaisilta, mutta niitä käyttämällä voit tehdä koodistasi huomattavasti tehokkaampaa ja välttyä turhalta koodin toistolta.

## Katso myös

- [PHP.net: Preg_match](https://www.php.net/manual/en/function.preg-match.php)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [MDN Web Docs: Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)