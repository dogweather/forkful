---
title:                "Merkkijonon suurennus"
html_title:           "Swift: Merkkijonon suurennus"
simple_title:         "Merkkijonon suurennus"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

BB

##Mitä ja miksi?

Päättäjänä, merkkijonon eli tekstirivan ensimmäisen kirjaimen muuttaminen suureksi on tapa muuttaa kirjainkoko sisällössä. Tämä auttaa korostamaan tietyt osat tekstistä ja on hyödyllinen esimerkiksi käytettäessä nimiä tai otsikoita.

##Kuinka se tehdään?

Swiftiläisille tämä on helppoa. On olemassa valmiiksi määritelty metodi, joka tekee sen puolestasi. Se näyttää tältä:

```Swift
let teksti = "tervetuloa"
print(teksti.capitalized)
```

Tämä tulostaisi "Tervetuloa" konsoliin. Huomaa, että metodi ei muuta alkuperäistä merkkijonoa, vaan palauttaa uuden version, joka on muutettu haluttuun muotoon.

##Syvemmälle

Capitalize-metodin lisäksi on olemassa myös muita tapoja muuttaa merkkijonon kirjainkokoa, kuten uppercased ja lowercased. Ne toimivat vastaavasti, mutta muuttavat koko merkkijonoa kokonaisuudessaan.

Uppercase ja lowercase perustuvat historiallisesti kirjaintyhdistelmään ASCIISTA, joka määritteli jokaiselle kirjaimelle numeron. Esimerkiksi iso A on numerolla 65 ja pieni a on numerolla 97. Näiden numeroiden avulla tietokone pystyi "kipuamaan" merkkijonoa ylös ja alas.

##Katso myös

Voit lukea lisää merkkijonosta ja sen muokkaamisesta Swiftin dokumentaatiosta: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html. Tutustu myös muihin merkkijonoihin liittyviin metodeihin, kuten replacingOccurrences ja trimmingCharacters.

Enjoy the journey of coding with Swift!