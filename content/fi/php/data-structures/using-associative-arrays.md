---
title:                "Assosiatiivisten taulukoiden käyttö"
date:                  2024-01-30T19:12:22.965836-07:00
model:                 gpt-4-0125-preview
simple_title:         "Assosiatiivisten taulukoiden käyttö"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

PHP:n assosiatiiviset taulukot ovat kuin tehostettuja listoja, joissa jokaista alkiota voidaan käyttää ihmisen luettavalla avaimella numeroiden sijaan. Ohjelmoijat käyttävät niitä tietojen tallentamiseen ja manipulointiin intuitiivisemmin, mikä mahdollistaa helpommin luettavan ja ylläpidettävämmän koodin.

## Kuinka:

PHP:ssä assosiatiivisten taulukoiden luominen ja käyttäminen on suoraviivaista. Tässä nopea yhteenveto:

```PHP
<?php
// Assosiatiivisen taulukon luominen
$henkilo = array(
    "nimi" => "John Doe",
    "ika" => 30,
    "sahkoposti" => "john@example.com"
);

// Vaihtoehtoisesti lyhyt taulukkosyntaksi
$henkilo = [
    "nimi" => "John Doe",
    "ika" => 30,
    "sahkoposti" => "john@example.com"
];

// Arvojen käyttäminen avaimilla
echo "Nimi: " . $henkilo["nimi"] . "\n";
echo "Ikä: " . $henkilo["ika"] . "\n";
echo "Sähköposti: " . $henkilo["sahkoposti"] . "\n";

// Arvon muuttaminen
$henkilo["ika"] = 31;

// Uuden avain-arvoparin lisääminen
$henkilo["maa"] = "USA";

// Iterointi assosiatiivisen taulukon yli
foreach ($henkilo as $avain => $arvo) {
    echo $avain . ": " . $arvo . "\n";
}

// Tuloste
// Nimi: John Doe
// Ikä: 31
// Sähköposti: john@example.com
// maa: USA
?>
```

Huomaa, miten avaimet voivat olla mitä tahansa merkkijonoja, mikä mahdollistaa elementtien käytön näiden avainten avulla numeeristen indeksien sijaan, jotka voivat olla vähemmän merkityksellisiä ja vaikeampia muistaa.

## Syväsukellus

PHP:n assosiatiiviset taulukot on toteutettu sisäisesti hajautustaulukoiden avulla, jotka tarjoavat erittäin nopean pääsyn elementteihin avaimen perusteella, mikä tekee niistä erittäin tehokkaita moniin tehtäviin. Tämä tehokkuus, yhdistettynä niiden käytön helppouteen, tekee assosiatiivisista taulukoista PHP-ohjelmoinnin kulmakiven.

Historiallisesti PHP:n taulukot (sekä indeksoitu että assosiatiivinen) ovat olleet uskomattoman joustavia, mikä on mahdollistanut niiden käytön listoina, pinoina, jonoissa ja muussa. Tämä joustavuus voi kuitenkin joskus johtaa tehottomampaan koodiin, jos sitä ei käytetä harkitusti.

Viime aikoina, parannusten myötä PHP:n oliopohjaisessa ohjelmoinnissa, jotkut kehittäjät suosivat olioiden käyttöä jäsennellyille tiedoille, erityisesti monimutkaisten tai keskinäisriippuvaisten tietoaineistojen kohdalla. Luokkien käyttö voi tarjota paremman kapseloinnin ja abstraktion, tehdä koodista helpommin testattavan ja selventää tarkoituksia. Kuitenkin yksinkertaisessa avain-arvo tallennuksessa ja suoraviivaisissa tiedonkäsittelytilanteissa assosiatiiviset taulukot säilyttävät erinomaisen valintansa yksinkertaisuuden ja intuitiivisen syntaksin ansiosta.
