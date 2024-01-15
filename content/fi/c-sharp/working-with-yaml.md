---
title:                "Työskentely yamlin kanssa"
html_title:           "C#: Työskentely yamlin kanssa"
simple_title:         "Työskentely yamlin kanssa"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

YAML (YAML Ain't Markup Language) on tiedostomuoto, jossa informaatio on tallennettu tavallisessa tekstimuodossa. Se on helppolukuinen ja ymmärrettävä, mikä tekee siitä suosittun ohjelmointikielten välisenä tietokantana. YAML:lla on myös laaja käyttö erilaisten konfiguraatiotiedostojen, kuten web-sovellusten ja tietokantojen, käsittelyssä.

## Kuinka

YAML-tiedostojen käsittely C#-ohjelmoinnissa on helppoa ja vaivatonta. Tässä muutama esimerkki koodinpätkä, joita voi hyödyntää YAML-tiedostojen lukemiseen ja kirjoittamiseen.

### YAML-tiedoston lukeminen

```C#
using System.IO; 
using YamlDotNet.RepresentationModel;

//Avataan YAML-tiedosto lukemista varten
var yamlStream = new YamlStream();
using (var reader = new StreamReader("tiedostonimi.yml")) 
{
    yamlStream.Load(reader);
}
            
//Haetaan dokumentti YAML-tiedostosta
var yamlDocument = yamlStream.Documents[0];

//Haetaan YAML-tiedoston juuri, jonka alle tiedot on tallennettu
var rootNode = yamlDocument.RootNode;

//Haetaan arvojuuri (Scalar) ja tulostetaan sen sisältö
var valueRoot = (YamlScalarNode)rootNode;
Console.WriteLine("Arvo: " + valueRoot.Value);
```

### YAML-tiedoston kirjoittaminen

```C#
using YamlDotNet.RepresentationModel;

//Luodaan uusi YAML-tiedosto
var yamlStream = new YamlStream();
yamlStream.Add(new YamlDocument());

//Lisätään tietoa tiedoston juureen
var rootNode = (YamlMappingNode)yamlStream.Documents[0].RootNode;
rootNode.Add("nimi", "Johanna");
rootNode.Add("syntymävuosi", 1988);

//Kirjoitetaan tiedosto
using (var writer = new StreamWriter("tiedostonimi.yml")) 
{
    yamlStream.Save(writer, false);
}

```

### Navigointi YAML-tiedostossa

YAML-tiedostoissa tieto on tallennettu hierarkkisesti, joten tiedostoja käsitellessä on tärkeää pystyä navigoimaan oikeisiin kohtiin. Tässä muutama esimerkki navigoinnista ja tietojen haku YAML-tiedostosta.

```C#
using YamlDotNet.RepresentationModel;

//Haetaan allekomentodi YAML-tiedostosta
var subordinateCommands = (YamlMappingNode)rootNode["allekomennot"];

//Haetaan tietty alikomento syntaksilla
var firstCommand = subordinateCommands["eka_alikomento"];

//Haetaan useamman alikomennon joukko
var commands = (YamlSequenceNode)rootNode["komennot"];
foreach (var command in commands)
{
    //Tulostetaan komennon nimi ja kuvaus
    var name = command["nimi"];
    var description = command["kuvaus"];
    Console.WriteLine("Nimi: " + name);
    Console.WriteLine("Kuvaus: " + description);
}
```

## Syvempi sukellus

YAML on erittäin monipuolinen tiedostomuoto, joten sen ominaisuuksia ja käyttömahdollisuuksia on paljon. Tarkemman tiedon ja dokumentaation YAML:sta löydät osoitteesta https://yaml.org/.

## Katso myös

- https://yaml.org/
- https://dotnet.microsoft.com/
- https://www.c-sharpcorner.com/technologies/yaml