---
title:                "Kotlin: Tiedostotiedoston kirjoittaminen"
simple_title:         "Tiedostotiedoston kirjoittaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Miksi kirjoittaa tekstitiedosto?

Tekstitiedostojen kirjoittaminen on yksi yleisimmistä ohjelmointitehtävistä monilla ohjelmointikielillä, mukaan lukien Kotlinilla. Tekstitiedostoja voidaan käyttää tallentamaan käyttäjän antamia tietoja, kuten lomakkeiden täyttämistä tai yksinkertaisen tekstin tallentamista. Ne voivat myös olla osa isompaa ohjelmaa, jossa ne toimivat tiedon tallennus- tai käyttöliittymäosana. Eli lyhyesti sanottuna, tekstitiedostoja kirjoitetaan usein siitä syystä, että ne tarjoavat kätevän tapa tallentaa tietoa ohjelmoinnin yhteydessä.

# Kuinka kirjoittaa tekstitiedosto Kotlinilla

Kotlinilla tekstitiedoston kirjoittaminen on helppoa ja yksinkertaista. Alla on esimerkki siitä, kuinka voit luoda uuden tiedoston ja kirjoittaa siihen yksinkertaisen tekstin. 

```Kotlin 
val tiedosto = File("tekstitiedosto.txt")
//luo uuden tiedoston nimeltä tekstitiedosto.txt

try {
    PrintWriter(tiedosto).use { out ->
        out.println("Tämä on esimerkki tekstistä, joka kirjoitetaan tiedostoon")
    }
} catch (e: IOException) {
    println("Virhe tiedoston kirjoittamisessa: $e")
}
```

Kun suoritat tämän koodin, se luo tekstitiedoston ja kirjoittaa siihen annetun tekstin. Tämä on vain yksinkertainen esimerkki, ja voit muokata sitä tarpeidesi mukaan.

# Syventyvä kurkistus tekstitiedoston kirjoittamiseen

Tekstitiedostot voivat sisältää muutakin kuin vain yksinkertaista tekstiä. Voit esimerkiksi tallentaa käyttäjän antamia tietoja tiedostoon ja lukea niitä myöhemmin käyttämällä `Scanner`-luokkaa. Voit myös kirjoittaa tiedostoon muun tyyppistä tietoa, kuten numerosarjoja tai merkkijonoja. Lisäksi voit käyttää `BufferedWriter`-luokkaa, joka tarjoaa paremman suorituskyvyn ja toimii erinomaisesti suurten tiedostojen kanssa.

Kotlinilla on monia mahdollisuuksia ja tapoja kirjoittaa tiedostoja. Suosittelemme tutustumaan Kotlinin viralliseen dokumentaatioon saadaksesi lisätietoa ja kokeilemaan eri tapoja kirjoittaa tiedostoja.

# Katso myös

Tässä blogipostauksessa esitellyt esimerkit ja koodipohjat ovat saatavilla GitHub-repositoriosta osoitteesta [linkki](https://github.com/kotlinguru/Kotlin-Blogi).

Voit myös lukea lisää tekstitiedostojen kirjoittamisesta Kotlinilla JetBrainsin verkkosivuilta: [linkki](https://kotlinlang.org/docs/tutorials/kotlin-for-py/writing-to-a-file.html).