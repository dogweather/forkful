---
title:                "Työskentely yamlin kanssa"
html_title:           "Java: Työskentely yamlin kanssa"
simple_title:         "Työskentely yamlin kanssa"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
YAML on tiedostomuoto tiedon tallentamiseen ja jakamiseen ohjelmointiprojekteissa. Kehittäjät käyttävät YAML:ää, koska se on yksinkertainen, helposti luettavissa ja joustava.

## Miten:
Käyttämällä Java-kirjastoa nimeltä "SnakeYAML" voimme lukea ja kirjoittaa YAML-tiedostoja Java-ohjelmassamme. Katso esimerkit alla ja niiden tuottama tulos.

```Java
Yaml yaml = new Yaml(); //luo YAML-olio
String data = "nimi: Jaska\nikä: 25"; //esimerkkitiedot YAML-muodossa
Map<String, Object> map = yaml.load(data); //muuntaa tiedot Map-muotoon
System.out.println(map.get("nimi")); //tulostaa "Jaska"
System.out.println(map.get("ikä")); //tulostaa "25"
```

```Java
Person henkilö = new Person("Maija", 30); //luo henkilö-olio
Yaml yaml = new Yaml(); //luo YAML-olio
String data = yaml.dump(henkilö); //muuntaa tiedot YAML-muotoon
System.out.println(data); //tulostaa "nimi: Maija\nikä: 30"
```

## Syvällinen sukellus:
YAML syntyi vuonna 2001 ja sen tarkoituksena oli korvata XML monimutkaisena tiedostomuotona. Sittemmin YAML:ää on käytetty mm. konfiguraatiotiedostoina ja datan tallentamiseen tietokantoihin. Vaihtoehtoisia tiedostomuotoja ovat esimerkiksi JSON ja CSV. SnakeYAML-kirjasto on suorituskykyinen, mutta sillä on myös muita vaihtoehtoja, kuten JYaml.

## Katso myös:
- [SnakeYAML Github-sivusto](https://github.com/snakeyaml/)
- [YAML-spesifikaatio](https://yaml.org/spec/)
- [JYaml-projekti](https://code.google.com/archive/p/jyaml/)