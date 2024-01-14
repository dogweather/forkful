---
title:                "C#: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

# Miksi käyttää YAML-muotoa C# ohjelmoinnissa?

YAML (Yet Another Markup Language) on helppokäyttöinen tapa tallentaa ja jakaa tietoa JSON- tai XML-muodon sijaan. Se on suunniteltu ihmisille luettavaksi ja kirjoittamiseen, mikä tekee siitä hyvin suositun monissa ohjelmointiympäristöissä.

## Miten käyttää YAML-muotoa C# ohjelmoinnissa

YAML-muotoa voidaan käyttää C# ohjelmoinnissa käyttämällä sopivaa kirjastoa, kuten esimerkiksi YamlDotNet. Esimerkiksi voimme tallentaa tietojen objekteja YAML-muodossa käyttämällä YamlSerializer-luokkaa seuraavalla tavalla:

```C#
// Luodaan objekti, jota haluamme tallentaa
MyClass obj = new MyClass();
obj.Name = "Johannes";
obj.Age = 30;

// Luodaan tallennuspaikka
string path = "C:\\tiedosto.yaml";

// Tallennetaan objekti YAML-muodossa
YamlSerializer serializer = new YamlSerializer();
using (TextWriter writer = File.CreateText(path))
{
    serializer.Serialize(writer, obj);
}
```

Yllä olevassa esimerkissä, YamlSerializer-luokka tallentaa objektin `MyClass` tiedostoon "tiedosto.yaml". Tämän jälkeen voimme helposti lukea ja käsitellä tallennettuja tietoja YAML-muodossa.

## Syväsukellus YAML-muodon maailmaan

YAML-muoto tarjoaa monia hyödyllisiä ominaisuuksia, kuten mahdollisuuden käyttää ankkurointia ja viittauksia, jolloin voidaan välttää tietojen toistaminen. Lisäksi YAML tukee myös monia erilaisia tietotyyppejä, kuten numeroita, merkkijonoja, listoja ja karttoja. Tämä tekee siitä erittäin monipuolisen ja joustavan tietojen tallennusmuodon.

Mikä parasta, YAML-muoto on myös helppo lukea ja kirjoittaa ihmisille, mikä helpottaa merkittävästi tietojen hallintaa ja jakamista tiimien välillä.

# Katso myös

- [YamlDotNet kirjasto](https://github.com/aaubry/YamlDotNet)
- [YAML-muoto virallisilla verkkosivuilla](https://yaml.org/)
- [JSON-muodon ja YAML-muodon vertailu](https://www.infoworld.com/article/3454742/json-vs-xml-vs-yaml-which-format-should-you-use-for-configuration-files.html)