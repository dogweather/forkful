---
title:                "Bash: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/working-with-csv.md"
---

{{< edit_this_page >}}

# Miksi käyttää CSV-tiedostoja Bash-ohjelmoinnissa

CSV-tiedostot ovat yleinen tapa tallentaa ja jakaa taulukkodataa. Niitä käytetään usein esimerkiksi Excel-taulukoissa ja verkkosivustojen tietokantayhteyksissä. Bash-ohjelmointikielen avulla voit käsitellä näitä tiedostoja tehokkaasti ja automatisoida tietojen käsittelyä.

## Miten tehdä se

Bashilla voit lukea ja kirjoittaa CSV-tiedostoja käyttämällä read-, write- ja IFS-komentoja. IFS-komennolla voit määrittää tietojen erottimen, jotta Bash osaa jakaa sarakkeet oikein. Alla on esimerkki, jossa luodaan uusi CSV-tiedosto ja kirjoitetaan siihen tietoja.

```Bash
# Luodaan uusi tiedosto "data.csv"
touch data.csv

# Määritetään erottimena pilkku
IFS=,

# Kirjoitetaan tiedostoon tietoja (sarakkeiden välissä on pilkku)
echo "Nimi, Ikä, Ammatti" >> data.csv
echo "Maija, 30, Sairaanhoitaja" >> data.csv
echo "Pekka, 42, Insinööri" >> data.csv

# Luetaan tiedostosta tietoja ja tulostetaan ne näytölle
read -a data < data.csv
echo "Nimi: ${data[0]}, Ikä: ${data[1]}, Ammatti: ${data[2]}"
```

Tämä koodi luo uuden CSV-tiedoston ja kirjoittaa siihen kolme riviä tietoja. Lopuksi se lukee tiedoston ja tulostaa sen sisällön. Voit myös muokata tietoa käyttämällä `sed`-komennon avulla.

## Syventävä tieto

CSV-tiedostojen käsittely Bash-ohjelmoinnissa voi vaikuttaa aluksi haastavalta, mutta kun oppii käyttämään read-, write- ja IFS-komentoja, pääsee alkuun. On myös tärkeää muistaa, että CSV-tiedostojen käsittelyyn voidaan käyttää myös muita ohjelmointikieliä, kuten Pythonia ja Javascriptiä.

See Also:

- [Bashin opas CSV-tiedostojen käsittelyyn](https://www.linuxjournal.com/content/csv-file-processing-using-bash)
- [Esimerkkejä CSV-tiedostojen käytöstä Bashilla](https://linuxconfig.org/reading-csv-file-in-bash)