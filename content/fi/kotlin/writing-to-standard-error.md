---
title:    "Kotlin: Tiedon kirjoittaminen vakiovirheeseen"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Miksi Standardiovirheeseen Kirjoittaminen On Hyödyllistä

On monia tilanteita, joissa haluamme tulostaa ohjelmamme virheilmoituksia suoraan terminaaliin. Tämä säästää aikaa ja vaivaa, kun virheiden etsiminen ja korjaaminen on välttämätöntä. Standardiovirheeseen kirjoittaminen onkin hyödyllinen tapa parantaa ohjelmamme virheenkäsittelyä ja helpottaa vianjäljitystä.

## Miten Kirjoitat Standardiovirheeseen Kotlinilla

Kotlinilla on yksinkertainen tapa kirjoittaa virheilmoituksia standardiovirheeseen. Käytä ensin `System.err.println()` -metodia ja anna sitten virheilmoituksesi sisällöstä parametriksi. Voit käyttää myös `err` -nimistä muuttujaa, joka viittaa standardiovirheeseen ja käyttää sen `println()`-metodia.

```Kotlin
System.err.println("Virhe: nimi ei voi olla tyhjä")
```

Tämä koodi tulostaa virheilmoituksen terminaaliin esimerkkinä virheestä, joka tapahtuu, jos käyttäjä syöttää tyhjän nimen.

```
Virhe: nimi ei voi olla tyhjä
```

## Syvempää Tutkimusta Standardiovirheisiin Kirjoittamisesta

Standardiovirheeseen kirjoittamisessa on tärkeää huomata, että virheilmoituksia ei yleensä haluta näkyvän käyttäjälle, vaan ne ovat tarkoitettu lähinnä ohjelmoijan käyttöön. Siksi on tärkeää käyttää `System.err` -luokkaa eikä `System.out` -luokkaa, joka tulostaa asioita standarditulostusvirtaan (terminaliin). Myös muista, että virheilmoitus tulee aina tulostaa ennen koodin suorittamista kyseisen virheen aiheuttaneelle riville.

Voit myös käyttää `java.util.logging` -pakettia kirjoittaaksesi tarkemman ja jäsennellymmän virheilmoituksen standardiovirheeseen. Tämä on erityisen hyödyllistä isommissa projekteissa, joissa on useampia ohjelmoijia tai haluat tallentaa virheilmoitukset esimerkiksi lokitiedostoihin.

## Katso Myös

- [Kotlinin virallinen dokumentaatio](https://kotlinlang.org/docs/tutorials/command-line.html)
- [Tarkempi opas virheiden käsittelemiseen Kotlinilla](https://www.baeldung.com/java-throw-custom-exception)
- [Java.util.logging-paketin opas](https://www.baeldung.com/java-logging-exceptions)