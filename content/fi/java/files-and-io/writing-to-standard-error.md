---
title:                "Kirjoittaminen standardivirheeseen"
aliases:
- /fi/java/writing-to-standard-error/
date:                  2024-02-03T19:33:45.938050-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kirjoittaminen standardivirheeseen"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Standardivirheeseen (stderr) kirjoittaminen tarkoittaa virheviestien ja diagnostiikkatietojen tulostamista konsoliin tai terminaaliin. Ohjelmoijat tekevät sen erotellakseen virhetiedot tavallisesta tulosteesta (stdout), mikä helpottaa debuggausta ja lokianalyysia.

## Kuinka:

### Perus stderr-tulostus Javassa
Java tarjoaa suoraviivaisen tavan kirjoittaa stderr:iin käyttäen `System.err.print()` tai `System.err.println()`. Näin se tehdään:

```java
public class StdErrExample {
    public static void main(String[] args) {
        try {
            int jako = 10 / 0;
        } catch (ArithmeticException e) {
            System.err.println("Virhe: Nollalla ei voi jakaa.");
        }
    }
}
```

Esimerkkitulo:

```
Virhe: Nollalla ei voi jakaa.
```

Tämä tulostaa virheviestin suoraan standardivirheen virtaan.

### Lokittaja Kehittyneeseen Virheenkäsittelyyn
Sovelluksille, jotka tarvitsevat kehittyneempää virheenkäsittelyä ja lokitusta, on yleistä käyttää lokituskirjastoa, kuten SLF4J yhdessä Logbackin tai Log4J2:n kanssa. Tämä mahdollistaa joustavamman virhetulosteiden hallinnan, mukaan lukien tiedostoon ohjaus, suodatus ja muotoilu.

#### Esimerkki Logbackin kanssa

Ensiksi, lisää riippuvuus Logbackille `pom.xml` (Maven) tai `build.gradle` (Gradle) tiedostoosi. Mavenille:

```xml
<dependency>
    <groupId>ch.qos.logback</groupId>
    <artifactId>logback-classic</artifactId>
    <version>1.2.3</version>
</dependency>
```

Sen jälkeen voit käyttää seuraavaa koodia lokatakseen virheet:

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggerExample {
    private static final Logger logger = LoggerFactory.getLogger(LoggerExample.class);
    
    public static void main(String[] args) {
        try {
            int tulos = 10 / 0;
        } catch (ArithmeticException e) {
            logger.error("Virhe: Nollalla ei voi jakaa.", e);
        }
    }
}
```

Tämä tulostaa virheviestin yhdessä pinorakenteen kanssa konsoliin tai tiedostoon riippuen Logbackin konfiguraatiosta.

Lokituskehyksien, kuten Logbackin, käyttö tarjoaa enemmän kontrollia virheenkäsittelyyn, mikä helpottaa suurien sovellusten ja järjestelmien hallintaa.
