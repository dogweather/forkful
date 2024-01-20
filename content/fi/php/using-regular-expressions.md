---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Bash: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? - "Mitä & Miksi?"
Käytämme säännöllisiä lausekkeita (regex) tekstiä käsitellessä, kun etsitään, tarkastetaan tai muokataan merkkijonoja. Ne säästävät aikaa ja tekevät monimutkaisista tehtävistä yksinkertaisia.

## How to: - "Kuinka:"
```PHP
<?php
// Etsitään puhelinnumeroa
$teksti = "Hei, soita minulle numeroon +358 40 1234567.";
$pattern = "/\+358 \d{2} \d{7}/";

if (preg_match($pattern, $teksti, $matches)) {
    echo "Puhelinnumero löydetty: " . $matches[0];
} else {
    echo "Numeroa ei löytynyt.";
}
// Näyttää: Puhelinnumero löydetty: +358 40 1234567

// Korvataan sähköpostiosoitteet
$korvattavaTeksti = "Ota yhteyttä meihin: esimerkki@osoite.com.";
$korvattuTeksti = preg_replace("/\b[\w\.-]+@[\w\.-]+\.\w{2,}\b/", "[sähköposti]", $korvattavaTeksti);
echo $korvattuTeksti;
// Näyttää: Ota yhteyttä meihin: [sähköposti].
?>
```

## Deep Dive - "Syväsukellus":
Regexien käyttö alkoi 1950-luvulla. Nykyisin ne ovat osa monia ohjelmointikieliä, PHP mukaan lukien `preg_*` funktioiden kautta. Vaihtoehtoisesti `strpos()` funktiota voidaan käyttää yksinkertaiseen etsintään, mutta se ei tarjoa regexien joustavuutta. Regexien toteutus PHP:ssa perustuu PCRE (Perl Compatible Regular Expressions) -kirjastoon, mikä takaa tehokkuuden ja monipuolisuuden.

## See Also - "Katso Myös":
- PHP Manual - Regular Expressions (Perl-Compatible): https://www.php.net/manual/en/book.pcre.php
- Regex101 - Interaktiivinen työkalu regexien testaamiseen ja opetteluun: https://regex101.com/
- Regexone - Interaktiivisia harjoituksia regexien oppimiseen: https://regexone.com/