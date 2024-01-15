---
title:                "Testien kirjoittaminen"
html_title:           "Kotlin: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi kirjoittaa testit osana ohjelmointia. Testien avulla pystyt varmistamaan koodisi toimivuuden ja välttämään mahdollisia virheitä jo ohjelmoinnin aikana. Lisäksi testien avulla pystyt helposti havaitsemaan muutokset koodissasi ja varmistamaan, että muutokset eivät ole aiheuttaneet ennalta-arvaamattomia ongelmia.

## Kuinka

Kirjoittaessasi testejä Kotlinilla, sinun tulee ensin tuoda JUnit-kirjasto osaksi projektitasi. Voit tehdä tämän lisäämällä `testImplementation("junit:junit:4.12")` riippuvuuden `build.gradle` tiedostoon. Seuraavaksi luot uuden Kotlin-tiedoston ja määrität testiluokan käyttäen `@Test` -annotaatiota. Alla on yksinkertainen esimerkki:

```Kotlin
import org.junit.Test

class CalculatorTest {
    @Test
    fun `returns the sum of two numbers`() {
        val result = add(2, 3)
        
        assertEquals(5, result)
    }
    
    fun add(num1: Int, num2: Int): Int {
        return num1 + num2
    }
}
```

Testiluokassa voit käyttää `assertEquals` metodia varmistaaksesi, että koodisi palauttaa halutun tuloksen. Voit myös käyttää muita JUnitin metodeita, kuten `assertTrue` tai `assertNotNull`, riippuen siitä mitä haluat testata.

## Syvemmällä

JUnitin lisäksi voit myös käyttää MockK-kirjastoa luodaksesi ja hallinnoimaan tekstimuotoisten testejen avulla. Voit myös käyttää Skipec-kirjastoa suorittaaksesi testejä vain tietyille Kodia ajamatta koko testiluokkaa. Lisäksi voit ottaa käyttöön Jacoco-pluginin seurataksesi testikattavuutta ja varmistaaksesi, että olet kattanut kaikki osat koodistasi testeillä.

## Katso myös

- [Kotlin Testing: A Practical Guide](https://www.raywenderlich.com/19147742-kotlin-testing-a-practical-guide) 
- [Unit Testing with JUnit and Kotlin](https://www.baeldung.com/kotlin/testing-with-junit-and-kotlin) 
- [Mocking in Kotlin with MockK](https://www.jetbrains.com/help/mockk/mocking-in-kotlin.html)