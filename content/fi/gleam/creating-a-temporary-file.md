---
title:                "Gleam: Väliaikaisen tiedoston luominen"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi?

Temporary files eli väliaikaistiedostot ovat tärkeitä osia ohjelmointia, koska ne mahdollistavat tiedon tallentamisen väliaikaisesti ilman pysyvää muutosta alkuperäisiin tiedostoihin.

## Miten?

Miten sitten luoda väliaikaistiedosto Gleam kielellä? Tässä muutama esimerkki ja havainnollistava tulostaulukko:

```Gleam
import filenames
import files

// Luodaan ensin väliaikainen tiedostonimi
let temp_file_name = filenames.temp()

// Luodaan väliaikainen tiedosto ja tallennetaan siihen merkkijono
let temp_file = files.write(temp_file_name, "Tämä on väliaikainen tiedosto")

// Luetaan väliaikaisen tiedoston sisältö
let contents = files.read(temp_file)

// Tulostetaan sisältö konsoliin
print!("Väliaikaisen tiedoston sisältö: {}", contents)

// Poistetaan väliaikainen tiedosto
files.delete(temp_file_name)
```

Tulostaulukko:

| Väliaikaisen tiedoston sisältö: | Tämä on väliaikainen tiedosto |

## Syvempi sukellus

Väliaikaisten tiedostojen luominen Gleam kielellä vaatii tiedostojen ja tiedostonimien moduulien tuomista, kuten yllä olevassa koodiesimerkissä nähdään. Tämä tapahtuu "import" komennolla. Tämän jälkeen voidaan käyttää tiedostonimien "temp()" funktiota luomaan uniikki väliaikainen tiedostonimi. Tämän jälkeen voidaan käyttää "files" moduulin "write()" funktiota tallentamaan haluttu sisältö väliaikaiseen tiedostoon ja "read()" funktiota lukemaan sisältö. Lopuksi tiedostojen moduulin "delete()" funktiolla voidaan poistaa väliaikainen tiedosto.

## Katso myös

- [Gleam kielikirjasto](https://gleam.run/documentation/#std-lib-files)
- [Gleam tiedostonimien moduuli](https://gleam.run/documentation/#std-lib-filenames)
- [Gleam tiedostojen moduuli](https://gleam.run/documentation/#std-lib-files)