---
title:                "Työskentely yaml:n kanssa"
html_title:           "PHP: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet PHP-ohjelmoija, joka etsii keinoa tallentaa ja lukea tietoja rakenteellisessa ja helposti luettavassa muodossa, YAML voi olla hyvä vaihtoehto. Se on yksinkertainen ja suosittu tapa käsitellä tietoja, ja sen käyttö voi helpottaa työtäsi.

## Miten tehdä tämä

```PHP 
<?php
// Tiedoston avaaminen ja tiedon tallentaminen YAML-muotoon
$auki = fopen('tiedosto.yml', 'w');
fwrite($auki, yaml_emit($tiedot));
fclose($auki);
// Tiedon lukeminen YAML-tiedostosta
$tiedot = yaml_parse_file('tiedosto.yml');
// Tietojen tulostus
print_r($tiedot);
```

Esimerkissä käytetään ```yaml_emit()``` ja ```yaml_parse_file()``` -funktioita, joilla voi tallentaa ja lukea tietoja YAML-muodossa. Funktiot ovat osa PHP:n vakioasennusta, joten ylimääräisiä asennuksia ei tarvita. YAML-tiedostot näyttävät samalta kuin taulukot ja ne ovat helppoja muokata.

## Syvemmälle

YAML eli "YAML Ain't Markup Language" on tierakenteinen kieli, jota käytetään tallentamaan ja välittämään tietoja ihmisluettavassa muodossa. Sen syntaksi perustuu sisennyksiin ja kolmelle erityiselle merkille ```-```, ```?```, ja ```:```. YAML sopii hyvin esimerkiksi konfigurointitiedostoihin ja yksinkertaisten tietomallien tallentamiseen.

YAML:n käyttö PHP:ssa on helppoa, sillä funktiot ```yaml_emit()``` ja ```yaml_parse_file()``` tekevät työn puolestasi. Voit myös käyttää muita PHP:n YAML-tukea tarjoavia kirjastoja, kuten Symfony YAML-kirjastoa.

## Katso myös

- [PHP:n YAML-dokumentaatio](https://www.php.net/manual/en/book.yaml.php)
- [Symfony YAML -kirjasto](https://symfony.com/doc/current/components/yaml.html)