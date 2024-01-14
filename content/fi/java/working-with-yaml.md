---
title:                "Java: Työskentely yamlin kanssa"
simple_title:         "Työskentely yamlin kanssa"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

YAML on loistava tapa tallentaa ja jakaa tietoa eri ohjelmistojen välillä. Joten, miksi ei hyödyntäisi tätä kätevää tiedostomuotoa myös Java-ohjelmoinnissa? 

## Miten

YAML-tiedostojen käsittely Java-ohjelmoinnissa on hyvin yksinkertaista. Seuraavien esimerkkien avulla saatat ymmärtää, miten YAML-tiedostoja voidaan lukea ja kirjoittaa Java-koodilla.

```Java
// Kirjoitetaan YAML-tiedosto
try {
    // Luodaan uusi YAML-tiedosto ja avataan kirjoitusmoodiin
    Yaml yaml = new Yaml();
    FileWriter writer = new FileWriter("tiedosto.yaml");
    BufferedWriter bufWriter = new BufferedWriter(writer);

    // Luodaan lista avaimia ja arvoja
    Map<String, String> data = new HashMap<>();
    data.put("nimi", "Matti");
    data.put("ika", "32");
    data.put("kaupunki", "Helsinki");

    // Kirjoitetaan lista YAML-muodossa tiedostoon
    yaml.dump(data, bufWriter);
    bufWriter.close();
} catch (IOException e) {
    e.printStackTrace();
}

// Luetaan YAML-tiedosto
FileReader reader = new FileReader("tiedosto.yaml");

// Käytetään YAMLParseria lukemaan tiedostoa
YAMLParser parser = new YAMLParser();
Object obj = parser.parse(reader);

// Muunnetaan objekti HashMapiksi
HashMap<String, String> data = (HashMap<String, String>) obj;

// Tulostetaan tiedoston sisältö
System.out.println("Nimi: " + data.get("nimi"));
System.out.println("Ikä: " + data.get("ika"));
System.out.println("Kaupunki: " + data.get("kaupunki"));
```

Tulostus:

```
Nimi: Matti
Ikä: 32
Kaupunki: Helsinki
```

## Syväsukellus

YAML-tiedostot koostuvat avaimista ja niitä vastaavista arvoista. Avaimet ovat aina merkkijonoja ja arvot voivat olla mitä tahansa dataa, esimerkiksi merkkijonoja, numeroita tai listoja. Ohjelmointikielenä Java tarjoaa monia eri tapoja käsitellä YAML-tiedostoja, kuten Map- ja HashMap-luokat sekä Yaml- ja YAMLParser-oliot.

YAML:ssa on myös mahdollista määritellä monimutkaisempia rakenteita, kuten taulukoita ja sisäkkäisiä avain-arvo pareja, joten sen avulla voi käsitellä monenlaisia tietomalleja. Se on myös helppo lukea ja muokata manuaalisesti, toisin kuin esimerkiksi JSON.

## Katso myös

- [YamlBeans](https://github.com/EsotericSoftware/yamlbeans) - Java-kirjasto YAML-tiedostojen käsittelyyn
- [Jackson-dataformat-YAML](https://github.com/FasterXML/jackson-dataformat-yaml) - JSON-tietojen muuntaminen YAML-muotoon Java-kirjastolla
- [YAML-tiedoston lukeminen ja muuttaminen Java-ohjelmassa](https://www.javacodegeeks.com/2018/09/parse-yaml-java.html) - Hyödyllisiä vinkkejä ja koodiesimerkkejä YAML-tiedoston käsittelyyn Java-ohjelmassa