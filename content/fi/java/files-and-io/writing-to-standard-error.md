---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:45.938050-07:00
description: "Standardivirheeseen (stderr) kirjoittaminen tarkoittaa virheviestien\
  \ ja diagnostiikkatietojen tulostamista konsoliin tai terminaaliin. Ohjelmoijat\
  \ tekev\xE4t\u2026"
lastmod: '2024-03-13T22:44:56.462016-06:00'
model: gpt-4-0125-preview
summary: Standardivirheeseen (stderr) kirjoittaminen tarkoittaa virheviestien ja diagnostiikkatietojen
  tulostamista konsoliin tai terminaaliin.
title: Kirjoittaminen standardivirheeseen
weight: 25
---

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
