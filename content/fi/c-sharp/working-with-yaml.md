---
title:                "Työskentely yaml:n kanssa"
html_title:           "C#: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
YAML tarkoittaa "YAML Ain't Markup Language" ja se on yksinkertainen ja luettava formaatti tietojen tallentamiseen. Se on hyödyllinen ohjelmoijille, koska se mahdollistaa tietojen tallentamisen rakenteisessa muodossa, jota voidaan helposti lukea ja muokata.

## Miten: 
Käytä seuraavaa koodin palaa tallentaaksesi merkkijonoja YAML-muodossa:

```C#
var yaml = @"name: John Smith
age: 30
address: 123 Main Street";
Console.WriteLine(yaml);
```

Tulostus:

```
name: John Smith
age: 30
address: 123 Main Street
```

Voit myös käyttää `YamlDotNet`-kirjastoa helpottaaksesi YAML-tiedostojen lukemista ja muokkaamista. Käytä seuraavaa koodia esimerkkinä:

```C#
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

var deserializer = new DeserializerBuilder().WithNamingConvention(CamelCaseNamingConvention.Instance).Build();
var yaml = @"name: John Smith
age: 30
address: 123 Main Street";

var person = deserializer.Deserialize<Person>(yaml);
Console.WriteLine(person.Name); // Output: John Smith
```

## Syvemmälle:
YAML luotiin ensimmäisen kerran vuonna 2001 ja sen tarkoituksena oli tarjota vaihtoehto XML-formaatille tietojen tallentamisessa. Siitä lähtien se on kasvanut suosituksi vaihtoehdoksi muille formaateille kuten JSON ja XML. YAML on myös helposti tuettu monilla ohjelmointikielillä, kuten C#.

### Vaihtoehtoja:
Jos haluat tallentaa tietoja rakenteisessa muodossa, voit myös käyttää JSON- tai XML-formaatteja. Kumpikin niistä toimii hyvin, mutta YAML on usein helpompi lukea ja kirjoittaa, kun kyseessä ovat suuremmat tiedostot.

### Toteutus:
YAML-muoto on peräisin Python-kielestä ja se tukee useita ohjelmointikieliä, mukaan lukien C#. YAML-formaatti on myös saatavana JavaScript- ja Java-kirjastoina, joten sen käyttö on mahdollista lähes kaikissa ohjelmointikielissä.

## Katso myös:
- [YAML-spesifikaatio] (https://yaml.org/)
- [YamlDotNet kirjasto] (https://github.com/aaubry/YamlDotNet)