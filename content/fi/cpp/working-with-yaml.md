---
title:                "Työskentely yaml:n kanssa"
html_title:           "C++: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?
YAML on tiedoston muoto, jota käytetään tietojen tallentamiseen hierarkkisessa muodossa. Se on suunniteltu helpoksi luettavaksi sekä ihmisille että ohjelmille. Ohjelmoijat käyttävät YAMLia helpottamaan tietojen tallentamista ja käsittelyä, sillä se on yksinkertainen ja tehokas muoto.

# Kuinka tehdä:
Käytä YAMLia tallentamaan tietoja Yamlcpp-kirjastolla. Voit aloittaa luomalla YAML-tiedoston käyttämällä ```C++ YAML::Emitter``` -luokkaa ja lisäämällä haluamasi tiedot. Voit myös käyttää ```C++ YAML::Node``` -luokkaa lukemaan ja muokkaamaan olemassa olevaa YAML-tiedostoa.

Esimerkiksi, voit tallentaa henkilön tiedot seuraavasti:
```
YAML::Emitter emitter;
emitter << YAML::BeginMap;
emitter << YAML::Key << "Nimi";
emitter << YAML::Value << "Matti";
emitter << YAML::Key << "Ikä";
emitter << YAML::Value << 30;
emitter << YAML::EndMap;
```

Tämä tuottaisi seuraavan YAML-tiedoston:
```
Nimi: Matti
Ikä: 30
```

Voit myös lukea YAML-tiedoston ja tulostaa sen sisällön seuraavasti:
```
YAML::Node node = YAML::LoadFile("tiedosto.yaml");
std::cout << node["Nimi"].as<std::string>() << std::endl;
std::cout << node["Ikä"].as<int>() << std::endl;
```

Tämä tulostaisi seuraavan:
```
Matti
30
```

# Syvemmällä:
YAML kehitettiin alun perin korvaamaan XML-tiedostojen monimutkainen syntaksi ja se on nykyään suosittu vaihtoehto tietojen tallentamiseen JSON-tiedostojen rinnalla. Se tukee myös monia ohjelmointikieliä, ei pelkästään C++:aa.

Jos tarvitset monimutkaisempia tietorakenteita, kuten taulukoita ja linkitystä, voit tutustua YAML-standardeihin ja käyttää niitä kirjastojesi kanssa. Voit myös käyttää erilaisia ​​YAML-editoria helpottamaan YAML-tiedostojen luomista ja muokkaamista.

Yamlcpp-kirjasto on avoimeen lähdekoodiin perustuva, joten voit löytää virheitä tai parantaa sen suorituskykyä lisäämällä ominaisuuksia. Voit myös tarkastella muita avoimen lähdekoodin YAML-kirjastoja, kuten LibYAML tai YAML-cpp, joilla on erilaisia ​​ominaisuuksia ja käytäntöjä.

# Katso myös:
- [Yamlcpp dokkari](https://github.com/jbeder/yaml-cpp/wiki)
- [YAML-standardeja](https://yaml.org/spec/)
- [YAML-editorit](https://yaml.org/resolver.html#yaml-editors)