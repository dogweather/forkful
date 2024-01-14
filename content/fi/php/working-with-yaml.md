---
title:                "PHP: Yhteistyössä yaml:n kanssa."
simple_title:         "Yhteistyössä yaml:n kanssa."
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi YAML:in käyttäminen on hyödyllistä?

YAML (Yet Another Markup Language) on helppolukuinen tietojen tallennusmuoto, joka on erityisen hyödyllinen PHP-ohjelmoijille. Sen avulla voit tallentaa ja jakaa tietoja yksinkertaisessa, tekstipohjaisessa muodossa. Tämä tekee YAML-in käytöstä erinomaisen vaihtoehdon tietokantojen ja monimutkaisten XML-tiedostojen sijaan.

## Kuinka käyttää YAML:ia PHP:n kanssa?

Aloitetaan yksinkertaisella esimerkillä. Oletetaan, että haluat tallentaa muistiinpanon "Ostoslista", joka sisältää erilaisia ruokakaupasta ostettavia tuotteita. Tämän voisi tallentaa YAML-tiedostoon seuraavalla tavalla:

```PHP
$tiedosto = "ostoslista.yml";
$muistiinpano = array(
 "Päivämäärä" => "2020-10-10",
 "Tuotteet" => array(
  "Leipä",
  "Maito",
  "Kananmunat",
  "Juusto",
  "Juuresmassa",
  "Jogurtti"
 )
);
file_put_contents($tiedosto, yaml_emit($muistiinpano));
```

Tässä ensin määritellään tiedoston nimi ja sen jälkeen luodaan muistiinpano muuttuja, joka sisältää päivämäärän ja tuote-listan. Lopuksi käytetään file_put_contents -funktiota, joka tallentaa muistiinpanon YAML-muodossa tiedostoon.

Voit myös ladata ja lukea YAML-tiedostoja PHP:ssa käyttämällä yaml_parse_file -funktiota:

```PHP
$tiedosto = "ostoslista.yml";
$ostoslista = yaml_parse_file($tiedosto);
echo "Ostoslista päivämäärällä: ".$ostoslista["Päivämäärä"]."\n";
echo "Tuotteet: \n";
foreach($ostoslista["Tuotteet"] as $tuote) {
 echo "- ".$tuote."\n";
}
```

Tämä tulostaa lähdetiedostossa määritellyn päivämäärän ja tuotelistan.

## Syvemmälle YAML:iin

YAML on puhtaasti tekstipohjainen formaatti, joka perustuu sisennyksiin. Sisennyksillä on siis suuri merkitys datan lukuun ja kirjoittamiseen. Sisennyksien tulee olla johdonmukaisia ja käyttää välilyöntejä, ei tabulaattoreita.

YAML:in avulla voidaan myös tallentaa monimutkaisempia tietorakenteita, kuten JSON:issa. Tällöin sisennyksillä ja luettelomerkkien avulla voidaan määritellä esimerkiksi listoja tai assosiatiivisia taulukoita.

## Katso myös

- [YAML-oppaat](https://yaml.org/start.html)
- [PHP-dokumentaatio YAML:stä](https://www.php.net/manual/en/book.yaml.php)
- [Symfony Yaml-komponentti](https://symfony.com/doc/current/components/yaml.html)