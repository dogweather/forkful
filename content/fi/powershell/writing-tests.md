---
title:                "Testien kirjoittaminen"
html_title:           "PowerShell: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/writing-tests.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Testaaminen on prosessi, jossa ohjelmoija varmistaa koodin toimivuuden ja virheettömyyden. Testaaminen on tärkeä osa ohjelmointia, koska se auttaa vähentämään bugien määrää ja parantaa ohjelman laatua.

## Näin teet sen:
PowerShell-ohjelmointikieli sisältää sisäänrakennetun unittest-moduulin, joka mahdollistaa testien kirjoittamisen koodin yhteyteen. Alla on esimerkki siitä, miten voit kirjoittaa yksinkertaisen testin:

```PowerShell
Describe "Luvun neliö" {
    It "Toimii oikein" {
        $luku = 5
        $tulos = $luku * $luku
        $odotettu = 25
        $tulos | Should -Be $odotettu
    }
}
```

Testin nimi annetaan avainsanalla *Describe*, jonka jälkeen avainsanan *It* avulla määritellään testin tarkoitus. Testissä annetaan muuttujille arvoja ja verrataan odotettua tulosta saatuun tulokseen *Should* -avainsanalla.

## Syvällisempi sukellus:
Testaukseen on olemassa myös muita menetelmiä kuten manuaalinen testaus tai käyttöliittymätestaus. Kuitenkin automaattiset yksikkötestit, kuten PowerShellin unittest-moduuli, ovat yleisesti käytettyjä ja luotettavia tapoja testata koodia. Testien kirjoittaminen myös auttaa kehittäjää ymmärtämään koodin toimintaa ja havaitsemaan mahdollisia virheitä.

## Katso myös:
Voit löytää lisätietoja PowerShellin unittest-moduulista ja testaamisesta yleisesti esimerkiksi seuraavista lähteistä:
- https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/differences-between-unit-tests-and-functional-tests?view=powershell-7
- https://powershellexplained.com/2017-03-21-Powershell-unit-testing-introduction/
- https://www.softwaretestinghelp.com/unit-testing-tutorial/