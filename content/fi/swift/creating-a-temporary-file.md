---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "Swift: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Luodessa ohjelmistoja, saatat törmätä tarpeeseen luoda väliaikaisia tiedostoja, myös kutsuttuna temp-tiedostoiksi. Nämä ovat tiedostoja, jotka ovat olemassa vain lyhyen aikaa ja poistuvat sitten automaattisesti, yleensä kun niitä ei enää tarvita. Tässä on pari esimerkkiä miksi ohjelmoijat saattavat luoda väliaikaisia tiedostoja: tallentaakseen väliaikaisia tietoja ja suorittaakseen tiettyjä toimintoja, kuten tiedostojen siirtoa tai tallentamista.

## Miten:

```Swift
let temporaryFile = try FileManager.default.url(for: .itemReplacementDirectory, in: .userDomainMask, appropriateFor: nil, create: true)
try "This is a temporary file".write(to: temporaryFile, atomically: true, encoding: .utf8)

// Tulostaa: Tämä on väliaikainen tiedosto
print(try String(contentsOf: temporaryFile))
```

## Syväsukellus:

Luominen väliaikaisia tiedostoja on ollut hyödyllinen tekniikka ohjelmointimaailmassa jo vuosikymmenien ajan. Se on myös yksi näppärä tapa suorittaa tiettyjä toimintoja ilman, että tarvitsee luoda pysyvää tiedostoa, jota et välttämättä tarvitse myöhemmin. Toinen vaihtoehto väliaikaisille tiedostoille on käyttää ns. "nsurlcache" -toiminnallisuutta, joka tallentaa ja hallinnoi väliaikaisia tiedostoja automaattisesti. Tämä on hyödyllistä esimerkiksi jos haluat tallentaa kuvia tai muita verkkosisältöjä väliaikaisesti. 

### Tiedostojen luominen väliaikaiseen hakemistoon:
Kun luot väliaikaisia tiedostoja, yksi tapa voi olla luoda niitä väliaikaiseen hakemistoon. Tämä hakemisto sijaitsee puhelimen tallentamassa tilassa, ja se on hyvä paikka tallentaa väliaikaisia tiedostoja, sillä niille on varattu erillinen tila, ja ne poistuvat automaattisesti. 

### Kirjoittaminen temp-tiedostoon:
Ohjataksesi sisältöä luomaasi temp-tiedostoon, tulee sinun ensin luoda "Data" -muotoinen esine, ja kirjoittaa haluamasi sisältö siihen "Data"(data.write(blaa, atomically: true)) -funktiolla. 

## Katso myös:

Lisätietoja temp-tiedostoista ja kuinka ne toimivat: https://developer.apple.com/documentation/foundation/filemanager/1407690-url
Lisätietoja NSURLCachesta: https://developer.apple.com/documentation/foundation/nsurlcache