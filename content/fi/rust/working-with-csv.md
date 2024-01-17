---
title:                "Csv:n käsittely"
html_title:           "Rust: Csv:n käsittely"
simple_title:         "Csv:n käsittely"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/working-with-csv.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?
CSV (Comma-Separated Values) eli pilkulla erotetut arvot on yleinen tapa tallentaa ja jakaa taulukkomuotoisia tiedostoja. Tämä on hyödyllistä ohjelmoijille, sillä se mahdollistaa tiedon siirtämisen eri ohjelmistojen välillä helposti ja kätevästi, ilman että tarvitsee huolehtia tiedostojen yhteensopivuudesta.

# Miten:
```Rust
// Avataan CSV-tiedosto ja luodaan uusi lukija
let file = std::fs::File::open("tiedosto.csv").unwrap();
let mut reader = csv::ReaderBuilder::new().from_reader(file);

// Käydään läpi jokainen rivi tiedostossa ja tulostetaan kentät
for result in reader.records() {
    let record = result.unwrap();
    for field in record.iter() {
        println!("{}", field);
    }
}
```
Esimerkkitulostus:
```bash
Rusting, on, hauskaa
Terve, maailma!
```

# Syvemmälle:
CSV muoto kehitettiin ensimmäisen kerran 1972 IBM:n toimesta ja siitä tuli nopeasti yleinen taulukkomuodon tallennusformaatti. Vaikka se on helppokäyttöinen ja tunnettu, on myös muita vaihtoehtoja kuten JSON ja XML. Rustin csv-kirjasto tarjoaa tehokkaan ja nopean tavan käsitellä CSV-tiedostoja, mutta on myös muita kirjastoja, kuten Serde, jotka tarjoavat samanlaisen toiminnallisuuden.

# Katso myös:
- [Rustin csv-kirjasto](https://crates.io/crates/csv)
- [Serde - kirjasto tietomuodon käsittelyyn](https://crates.io/crates/serde)
- [Kuinka käsitellä CSV-tiedostoja Rustilla](https://blog.logrocket.com/parsing-csv-files-in-rust-with-serde/)