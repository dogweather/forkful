---
title:                "Swift: Komentoriviparametrien lukeminen"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi lukea komentoriviparametrit?

Miksi haluaisit lukea komentoriviparametreja Swiftissä? On olemassa monia tilanteita, joissa nämä parametrit voivat olla hyödyllisiä. Esimerkiksi ohjelman suorittamiseen tarvitaan tiettyä tietoa, kuten käyttäjän antamia arvoja tai polkuja. Komentoriviparametrit tarjoavat yksinkertaisen tavan välittää näitä arvoja ohjelmalle ilman, että niitä tarvitsee kovakoodata itse koodiin. 

## Miten lukea komentoriviparametreja Swiftissä?

Onneksi Swiftin komentoriviparametrien lukeminen on melko yksinkertaista. Käytämme `CommandLine` luokkaa, joka tarjoaa valmiin rajapinnan komentoriviparametrien lukemiseen. Tässä on yksinkertainen esimerkki, joka tulostaa kaikki annetut parametrit:

```Swift
let arguments = CommandLine.arguments
print(arguments)
```

Jos ajamme tämän komennon terminaalissa antaen parametreiksi esimerkiksi `Swift example.swift -input hello -output world`, tulostuu seuraava tulos:

`["example.swift", "-input", "hello", "-output", "world"]`

Kuten näemme, komennon ensimmäinen parametri on aina tiedoston nimi itse, ja sen jälkeen tulevat käyttäjän antamat arvot. 

## Syvemmälle komentoriviparametreihin Swiftissä

`CommandLine` luokassa on myös muita hyödyllisiä metodeja parametrien kanssa työskentelyyn. Esimerkiksi `arguments`-ominaisuus palauttaa listan kaikista parametreista, kun taas `option`-ominaisuus antaa mahdollisuuden tarkistaa, onko tietty parametri annettu. Voimme myös käyttää `argumentCount`-ominaisuutta saadaksemme tiedon siitä, kuinka monta parametria on annettu. 

Tässä on yksinkertainen esimerkki, joka tulostaa käyttäjän antamien parametrien määrän:

```Swift
let count = CommandLine.argumentCount
print("Number of arguments: \(count)")
```

Jos ajamme tämän komennon esimerkin mukaisesti, ilmoittaa seuraava tulos meidän antaneen 5 parametria (`["example.swift", "-input", "hello", "-output", "world"]`). 

## Katso myös

- [Apple Developer: CommandLine Class](https://developer.apple.com/documentation/foundation/commandline)
- [Swift by Sundell: Working with the Command Line](https://www.swiftbysundell.com/articles/working-with-the-command-line-in-swift/)