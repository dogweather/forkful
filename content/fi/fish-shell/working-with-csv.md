---
title:                "Csv:n kanssa työskentely"
html_title:           "Fish Shell: Csv:n kanssa työskentely"
simple_title:         "Csv:n kanssa työskentely"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

CSV-tiedostot ovat yleisiä formaatteja, joita käytetään tietojen tallentamiseen ja jakamiseen. Trippeli pilkulla erotetut arvot (Comma-Separated Values) tekevät tiedostojen käsittelystä helppoa ja tehokasta. Tämän vuoksi monilla ohjelmoijilla on tarve työskennellä CSV-tiedostojen kanssa.

## Miten:

Fish Shell mahdollistaa CSV-tiedostojen käsittelemisen vähäisellä vaivalla. Alla on pari esimerkkiä:

```
# Lukee CSV-tiedoston rivi kerrallaan ja tulostaa sen
set csv_data (csvread -d ';' "example.csv")
for row in $csv_data
    echo $row
end

# Muokkaa CSV-tiedoston sisältöä ja tallentaa muutokset
set csv_data (csvread -d ';' "example.csv")
set new_row "John;Doe;john@doe.com"
set new_data (string join "\n" $csv_data $new_row)
echo $new_data > "example.csv"
```

## Syväsukellus:

CSV-tiedostot ovat olleet käytössä jo vuodesta 1972 lähtien ja ne ovat edelleen suosittuja tietojen tallentamiseen. Vaikka Fish Shell tarjoaa kätevän tavan käsitellä CSV-tiedostoja, on olemassa myös muita vaihtoehtoja, kuten awk ja sed. Fish Shellin sisäinen csvread-toiminto hyödyntää csvkit-kirjastoa, joka taas perustuu Pythonin csv-mooduliin.

## Katso myös:

Lisää tietoa Fish Shellin käyttämisestä CSV-tiedostojen kanssa löydät täältä: https://fishshell.com/docs/current/cmds/csvread.html