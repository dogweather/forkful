---
title:                "Työskentely yaml:n kanssa"
html_title:           "Rust: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

YAML-tiedostot ovat yleinen tapa tallentaa ja jakaa tietoja ohjelmistokehityksessä. Käyttämällä Rustia, voit käsitellä ja muokata YAML-tiedostoja tehokkaasti ja luotettavasti.

## Kuinka

YAML-tukea ei sisällytä Rustin standardikirjastoon, mutta se voidaan helposti lisätä kolmannen osapuolen kirjaston avulla. Tässä on esimerkki käytöstä serde_yaml -kirjaston avulla, jotta voit muuntaa YAML-tiedostot Rust-tietorakenteiksi:

```Rust
// Lisää riippuvuus Cargo.toml-tiedostoon
[riippuvuudet]
serde_yaml = "0.8"

// Tuo kirjasto
Käyttö serde_yaml::aukeama;

// Lue YAML-tiedosto ja muunna se rakenteeksi
puu = serde_yaml::from_str(r"---
nimet:
  - Mikko
  - Anna
  - Juuso")?;
```

Yllä olevassa esimerkissä luodaan puu-tyyppinen tietorakenne, jossa on "nimet" -kenttä, joka sisältää kolme nimeä. Voit käyttää samaa lähestymistapaa myös kirjoittaessasi Rust-tietorakenteita YAML-tiedostoiksi.

## Syvempi sukellus

serde_yaml -kirjasto käyttää serde -kirjastoa muuntamaan YAML-muodon tietorakenteiksi ja takaisin. Voit lisätä benett-yaml -kirjaston konfiguroimaan muunnetun YAMLin muodon ja tiedostorakenne kirjoitettavaksi. Tämä antaa sinulle enemmän hallintaa siitä, miten tiedostoja käsitellään ja varmistaa, että ne ovat yhteensopivia muiden ohjelmien kanssa, jotka käyttävät YAML-tiedostomuotoa.

## Katso myös

- [serde_yaml-dokumentaatio](https://docs.rs/serde_yaml/0.8.16/serde_yaml/)
- [benett-yaml-dokumentaatio](https://docs.rs/bennett-yaml/0.7.0/bennett_yaml/)
- [YAML-spesifikaatio](https://yaml.org/spec/1.2/spec.html)