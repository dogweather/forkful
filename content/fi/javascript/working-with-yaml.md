---
title:                "Työskentely yaml:n kanssa"
html_title:           "Javascript: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
YAML on tietokielto, jota käytetään datan tallentamiseen ja siirtämiseen erilaisissa ohjelmistokehityksen projekteissa. Ohjelmoijat käyttävät sitä, koska se mahdollistaa selkeän ja helposti luettavan datan esitystavan ja siten helpomman ymmärrettävyyden ja ylläpidettävyyden.

## Miten:
Käyttämällä JavaScriptiä ja sen tarjoamia työkaluja, voit helposti lukea ja kirjoittaa YAML-tiedostoja ohjelmassasi. Esimerkiksi, voit käyttää yaml-loader -moduulia Webpackin avulla saadaksesi YAML-tiedostojen datan suoraan JavaScript-objekteiksi käyttöösi. Alla on yksinkertainen esimerkki:

```JavaScript
var yaml = require('yaml-loader!');
var data = yaml.load('person:
  name: John
  age: 25');
console.log(data.person.name); //tulostaa "John"
```

Voit myös käyttää yamljs-moduulia suoraan JavaScriptissa, ilman Webpackia:

```JavaScript
var YAML = require('yamljs');
var data = YAML.load('person:
  name: John
  age: 25');
console.log(data.person.age); //tulostaa 25
```

## Syvemmälle:
YAML kehitettiin alun perin vuonna 2001 korvaamaan XML:ää yksinkertaisempana ja helpommin luettavana vaihtoehtona. Vaikka sen suosio on kasvanut vuosien varrella, on olemassa myös muita vaihtoehtoja tiedon tallentamiseen ja siirtämiseen, kuten JSON tai HJSON. Valinta riippuu käyttötapauksista ja mieltymyksistä.

YAML-tiedostot ovat rakenteeltaan erittäin yksinkertaisia, koska ne käyttävät loogisia sisennyksiä ja luettavissa olevaa syntaksia. Tämän ansiosta ne soveltuvat hyvin käytettäväksi konfiguraatiotiedostoina. Voit myös sisällyttää YAML-tiedostojen sisään HTML- tai Markdown-muotoilua, mikä tekee niistä erittäin monipuolisia.

## Katso myös:
- [YAML-spesifikaatio](http://www.yaml.org/spec/1.2/spec.html)