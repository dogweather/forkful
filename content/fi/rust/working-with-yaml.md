---
title:                "Työskentely yamlin kanssa"
html_title:           "Rust: Työskentely yamlin kanssa"
simple_title:         "Työskentely yamlin kanssa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
YAML (YAML Ain't Markup Language) on tiedostoformaatti, joka mahdollistaa datan tallentamisen selkeään ja luettavaan muotoon. YAML-tiedostot sisältävät tietoa tietorakenteista ja niitä käytetään usein konfigurointitiedostoina ohjelmistoissa. Ohjelmoijat käyttävät YAML:ää helpottaakseen datan tallentamista, lataamista ja käsittelyä ohjelmistoissaan.

## Miten?
Rustissa voit käyttää YAML:n käsittelyyn esimerkiksi ranke- tai serde-yaml-kirjastoja. Seuraavat esimerkit näyttävät, kuinka voit luoda ja lukea YAML-tiedostoja Rustin avulla.

```Rust
// Luo YAML-tiedosto ja tallenna siihen tietoa
let data = "
name: John
age: 30
hobbies:
- hiking
- reading
";

let mut file = File::create("tiedosto.yaml").unwrap();
file.write_all(data.as_bytes()).unwrap();

// Lukee YAML-tiedostosta tiedot ja tulostaa ne konsoliin
let f = File::open("tiedosto.yaml").unwrap();
let buf_reader = BufReader::new(f);

let yaml_value: Yaml = serde_yaml::from_reader(buf_reader).unwrap();
println!("{:?}", yaml_value);
```

## Syvemmälle
YAML kehitettiin helpottamaan datan käsittelyä ja tiedon siirtämistä ohjelmien välillä. Se on suosittu vaihtoehto XML-tiedostoille, sillä YAML on helpompi lukea ja kirjoittaa ihmisille. Rustissa on myös muita vaihtoehtoja YAML:n käsittelyyn, kuten yaml-rust ja yaml-rs, jotka myös tarjoavat erilaisia toiminnallisuuksia.

YAML-tiedosto koostuu avain-arvo pareista, jotka on jäsennetty sisennyksillä. Tämä tarkoittaa, että tiedosto on helposti luettavissa ja ymmärrettävissä. YAML-tiedostoa käytetään yleisesti esimerkiksi konfigurointitiedostoina ohjelmistoissa ja tietojen tallentamiseen tietokannoissa.

## Katso myös
- [Rustin virallinen dokumentaatio](https://doc.rust-lang.org/stable/book/ch11-00-testing.html)
- [Ross Cruickshank: Yaml-rust](https://github.com/rust-lang-nursery/yaml-rust)
- [Buster Neece: Yaml-rs](https://github.com/cybergeek94/yaml-rs)