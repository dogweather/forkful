---
title:                "Swift: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Temporary filejen luominen on olennainen osa Swift ohjelmointia. Se voi olla hyödyllistä silloin, kun halutaan tallentaa tilapäisiä tietoja tai kun käsitellään suuria tiedostoja, joita ei haluta säilyttää pysyvästi.

## Miten tehdä

Temporary filejen luominen Swift:ssä on helppoa ja nopeaa. Käytännössä tämä tapahtuu luomalla `URL`-muuttuja ja asettamalla se `temporaryDirectory`-metodin palauttamalle osoitteelle. Tämän jälkeen voimme käyttää tätä `URL`:aa tallentamaan ja käsittelemään tiedostoja.

```Swift
var temporaryFileURL = URL(fileURLWithPath: NSTemporaryDirectory())
temporaryFileURL.appendPathComponent("temporaryFile.txt")
print(temporaryFileURL.path)
```

Tämä koodi luo `URL`:n, joka viittaa temp-hakemistoon. Tämän jälkeen se lisää tiedoston nimen `temporaryFile.txt` URL:iin ja tulostaa lopullisen polun konsoliin.

## Syvällisempi tarkastelu

Temporary filejen luomiseen liittyy muutamia huomioita, joita kannattaa pitää mielessä. Ensinnäkin temp-hakemisto on ohjelmassa luodessa aina sama, mutta se tyhjennetään aina, kun laitetta sammutetaan. Tämän vuoksi filejen tallentaminen tänne ei ole suositeltavaa, jos tiedostoja halutaan säilyttää pidemmän ajan.

Toiseksi, `URL`:ien sijasta voidaan käyttää myös `FileManager`-luokkaa luomaan ja käsittelemään temporary fileja. Tämä tarjoaa hieman enemmän hienosäätömahdollisuuksia, kuten tiedostojen poistamisen ja nimien muuttamisen.

Lopuksi, on tärkeää muistaa poistaa temp filet ohjelman suorituksen loppuvaiheessa. `FileManager` tarjoaa tähän `removeItem(at:)`-metodin, jota voi käyttää temp filejen poistamiseen.

## Katso myös

- [Apple:n dokumentaatio temporary filejen luomisesta Swift:ssä](https://developer.apple.com/documentation/foundation/filemanager/1407699-urlfordeletingitem)
- [Swiftille luotu temporary filejen hallintakirjasto](https://github.com/randymarsh77/SwiftTempFileManager)