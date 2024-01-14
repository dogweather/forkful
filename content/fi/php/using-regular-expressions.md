---
title:                "PHP: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Regular expressions, eli säännölliset lausekkeet, ovat tärkeä osa PHP-ohjelmointia. Niiden avulla voit hakea, vertailla ja muokata merkkijonoja monimutkaisia sääntöjä noudattaen. Tämä tekee koodistasi tehokkaampaa ja joustavampaa.

## Miten

Regular expressionit kirjoitetaan yleensä merkkijonona, joka koostuu erilaisista symboleista ja erikoismerkeistä. Niiden avulla voit hakea haluamaasi merkkijonoa, esimerkiksi tietynlaista sähköpostiosoitetta tai URL-osoitetta.

Käytännössä regular expressionin luominen alkaa aina merkkijonosta ```/```, joka ilmaisee säännöllisen lausekkeen alun ja lopun. Sen jälkeen voit lisätä haluamasi säännöt välilyöntien avulla. Esimerkiksi, jos haluaisit hakea kaikki 10-numeroiset puhelinnumerot, voit käyttää seuraavaa regular expressionia: ```/\d{10}/```. Tämä tarkoittaa, että haluat hakea kaikki merkkijonot, jotka koostuvat 10 numerosta.

Voit myös käyttää erilaisia erikoismerkkejä, kuten ```*``` tai ```+```, joilla voit ilmaista esimerkiksi, että haluat hakea useampia samanlaisia merkkejä tai merkkijonoja. Lisäksi voit käyttää myös sulkumerkkejä ```()``` ryhmitelläksesi sääntöjä.

## Syvempi sukellus

Regular expressioneita voi käyttää monella eri tavalla. Voit esimerkiksi tarkistaa, että käyttäjän syöttämä sähköpostiosoite on oikeassa muodossa tai hakea kaikki tietokannasta tietynlaiset tiedot.

PHP:ssa on myös valmiita funktioita, kuten ```preg_match()``` tai ```preg_replace()```, joilla voit käyttää regular expressioneita helposti osana koodiasi. Näiden funktioiden avulla voit esimerkiksi tarkistaa, että annettu syöte vastaa haluttua muotoa tai korvata osan merkkijonosta toisella.

On tärkeää muistaa, että regular expressionien luominen ja käyttäminen voi tuntua aluksi vaikealta ja haastavalta. Mutta harjoitus tekee mestarin ja kun opit perusteet, huomaat pian kuinka hyödyllisiä ne ovat koodissasi.

## Katso myös

- [PHP:n virallinen ohjeistus regular expressioneista](https://www.php.net/manual/en/book.pcre.php)
- [Regular expression cheat sheet](https://www.rexegg.com/regex-quickstart.html)
- [O'Reilly:n kirja "Mastering Regular Expressions"](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)