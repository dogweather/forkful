---
title:                "Työskentely csv:n kanssa"
html_title:           "Gleam: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

# Miksi

CSV eli erotettu-arvot tiedosto on yleinen tapa tallentaa ja jakaa tietoa. Sen avulla voit tallentaa tietoa taulukkomuodossa, mikä tekee sen käyttämisestä helppoa ja tehokasta. Gleam-ohjelmointikielen avulla voit käsitellä CSV-tiedostoja nopeasti ja helposti.

# Miten

Lyhyesti, luo ensin yhteys CSV-tiedostoon Gleamin avulla käyttämällä avainmoduulia ja sen jälkeen käytä yhteiset CSV-funktiot tiedon hakemiseen ja käsittelyyn. Joten, jos haluat esimerkiksi lukea CSV-tiedostosta ja tulostaa sen sisällön konsolille, kirjoita seuraavasti:

```Gleam
tiedosto = "tiedosto.csv"
tulos = Csv.read_file(tiedosto)
case tulos {
Ok(tiedot) -> x Printf("Data: #{tiedot}")
Error(virhe) -> x Printf("Virhe: #{virhe}")
}
```

Tämä esimerkki lukee CSV-tiedoston nimeltä "tiedosto.csv" ja tulostaa sen sisällön konsolille. Tämä osoittaa, kuinka helposti Gleam-kieli käsittelee CSV-tiedostoja. Voit myös käyttää muita funktioita, kuten `Csv.write_file` tiedon tallentamiseen CSV-muodossa. Lisäksi Gleamin avulla voit suorittaa monimutkaisempia toimintoja, kuten tietojen muokkausta ja validointia.

# Syvälle sukellus

Gleam-ohjelmointikieli tarjoaa laajan valikoiman CSV-funktioita, jotka helpottavat CSV-tiedostojen käsittelyä. Voit esimerkiksi käyttää `Csv.parse_row` funktiota jakamaan yksittäiset rivit CSV-tiedostosta tai `Csv.format_row` funktiota luomaan uusia rivejä CSV-tiedostoon. Lisäksi, Gleam tarjoaa myös muita käteviä työkaluja, kuten `List.to_csv` ja `List.from_csv` helpottamaan muunnoksia CSV-tiedostojen ja muiden tietorakenteiden välillä.

# Katso myös

- Gleam-kielen virallinen dokumentaatio: https://gleam.run/
- Gleam-kielen CSV-moduulin dokumentaatio: https://gleam.run/modules/csv
- Pragmatismo-julkaisu, suomenkielinen artikkeli Gleam-kielestä: https://pragmatismo.news/2020/02/12/esittelyssa-gleam-ohjelmointikieli/