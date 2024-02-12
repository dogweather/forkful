---
title:                "Lokitus"
aliases: - /fi/kotlin/logging.md
date:                  2024-01-26T01:07:01.248634-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lokitus"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/logging.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Lokitus on pohjimmiltaan tapa kirjata tapahtumia ja tietoja ohjelmistosovelluksesta ulkoiseen lähteeseen, kuten tiedostoon tai konsolille. Ohjelmoijat kirjaavat asioita koodin jäljittämiseen, ongelmien ratkaisemiseen ja sovelluksen toiminnan tarkkailuun luonnossa, mikä tarjoaa kriittisiä näkemyksiä, joita ei voi yhtä tehokkaasti saada muulla tavalla.

## Kuinka:

Kotlinissa lokitusta voidaan tehdä sisäänrakennetulla `println()`-funktiolla yksinkertaisissa tapauksissa tai monimutkaisemmilla kirjastoilla kuten SLF4J Logbackin kanssa tai Log4j edistyneisiin tarpeisiin.

Alla on yksinkertainen esimerkki käyttäen `println()`-funktiota:

```Kotlin
fun main() {
    println("Yksinkertainen lokiviesti: Sovellus käynnistetty.")
    // ... jotain sovelluslogiikkaa tässä ...
    try {
        // Simuloi virhettä
        throw Exception("Simuloitu virhe")
    } catch (e: Exception) {
        println("Virhelokiviesti: " + e.message)
    }
}
```

Tuloste:
```
Yksinkertainen lokiviesti: Sovellus käynnistetty.
Virhelokiviesti: Simuloitu virhe
```

Ja tässä on pätkä käyttäen SLF4J:tä Logbackin kanssa konfiguroituna:

```Kotlin
import org.slf4j.LoggerFactory

private val logger = LoggerFactory.getLogger("MyAppLogger")

fun main() {
    logger.info("Rakenteellinen lokiviesti: Sovellus käynnistetty.")
    // ... jotain sovelluslogiikkaa tässä ...
    try {
        // Simuloi virhettä
        throw Exception("Simuloitu virhe")
    } catch (e: Exception) {
        logger.error("Rakenteellinen virheloki: ", e)
    }
}
```

Olettaen sopivan Logback-konfiguraation, tuloste olisi muotoiltu ja saattaisi näyttää tältä, kun se kirjoitetaan lokitiedostoon:
```
[INFO] - 2023-03-29 14:15:42 - MyAppLogger - Rakenteellinen lokiviesti: Sovellus käynnistetty.
[ERROR] - 2023-03-29 14:15:43 - MyAppLogger - Rakenteellinen virheloki: 
java.lang.Exception: Simuloitu virhe
   at com.myapp.Main.main(Main.kt:10)
```

## Syväsukellus

Perinteisesti ohjelmistojen lokitus on kehittynyt ohjelmien ja järjestelmien monimutkaistuessa. Yksinkertaiset tulostuslauseet riittivät varhaisiin päiviin, jolloin ohjelmia usein ajoivat ja debuggasivat kehittäjät itse. Mutta kun järjestelmät olivat verkotettuja ja ajoivat eri ympäristöissä eri käyttäjillä, tuli kestävästä ja pysyvästä lokitusjärjestelmästä ratkaisevan tärkeää.

Ennen kuin Kotlin tuli suosituksi, Java-kehittäjät ottivat laajalti käyttöön kirjastoja, kuten Log4j ja myöhemmin SLF4J. Nämä ovat innoittaneet vastaavia käytäntöjä Kotlinissa, hyödyntäen Kotlinin yhteentoimivuutta Java-kirjastojen kanssa. SLF4J toimii abstraktiotasona, jonka avulla todellista lokitusimplementaatiota voidaan vaihtaa – yleensä Logback tai Log4j2 ovat suosittuja valintoja.

Kotlin mahdollistaa myös monialusta lokitusratkaisut, jotka toimivat JVM:ssä, JavaScriptissä ja nativiivisti esimerkiksi `expect`-/`actual`-mekanismin kautta, joka abstrahoi alustakohtaiset toteutukset.

Verrattuna omistettuihin lokituskirjastoihin println jatkuu yksinkertaisimpana lokitusmuotona, koska se ei vaadi lisäasetuksia tai riippuvuuksia; kuitenkin, se on yleensä sopimaton tuotantosovelluksiin puuttuvien ominaisuuksien, kuten lokitasojen, lokikierron ja rakenteellisten muotojen, vuoksi.

Muita edistyneiden lokituskehikoiden yleisiä ominaisuuksia ovat:

- Lokitustasot (DEBUG, INFO, WARN, ERROR jne.) lokiviestien kiireellisyyden luokittelemiseksi.
- Tulostus eri kohteisiin, kuten konsoliin, tiedostoon, tietokantoihin tai verkkopalveluihin.
- Automaattinen lokien kierto ja säilytyskäytännöt.
- Jakautuneen jäljityksen tuki mikropalveluarkkitehtuurissa.
- Rakenteellinen lokitus käyttäen muotoja, kuten JSON, joka integroituu hyvin lokianalytiikkajärjestelmiin.

Nämä työkalut ja ominaisuudet ovat ratkaisevan tärkeitä luotettavan ja havainnoitavan järjestelmän ylläpidossa erityisesti monimutkaisissa, jakautuneissa tai korkeasti skaalautuvissa ympäristöissä.

## Katso Myös

Lisätietoja ja oivalluksia Kotlin-lokituksesta löydät:

- SLF4J (Simple Logging Facade for Java) [http://www.slf4j.org/](http://www.slf4j.org/)
- Logback, Log4j:n seuraaja [http://logback.qos.ch/](http://logback.qos.ch/)
- Log4j 2 [https://logging.apache.org/log4j/2.x/](https://logging.apache.org/log4j/2.x/)
- Kotlin Multiplatform -dokumentaatio 'expect' ja 'actual' -julistuksista: [https://kotlinlang.org/docs/multiplatform.html](https://kotlinlang.org/docs/multiplatform.html)
- Opas rakenteellisesta lokituksesta Kotlinissa: [https://ktor.io/docs/logging.html](https://ktor.io/docs/logging.html)
