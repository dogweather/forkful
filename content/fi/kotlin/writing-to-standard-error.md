---
title:                "Kirjoittaminen vakiovirheeseen"
date:                  2024-01-19
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Standard error (stderr) on kirjoitusväylä virhetietojen raportointiin. Käytetään erottamaan ohjelman tuloste virhetiedostoista ja diagnosoimaan ongelmia. 

## How to:
Kotlinissa stderriin kirjoittaminen hoituu `System.err`-olion avulla.

```kotlin
fun main() {
    System.err.println("Tämä on virheviesti")
}
```

Ohjelma tulostaa:
```
Tämä on virheviesti
```

## Deep Dive
Alussa käyttöjärjestelmissä oli tapana erottaa normaali tuloste (stdout) ja virhetuloste (stderr) toisistaan joustavuuden vuoksi. Tiedostojärjestelmäoperaatioissa voit johtaa stderrin esim. tiedostoon tai muille prosesseille. Standardiin virheeseen kirjoittamisen toteuttaminen on kielen ja alustan sisäänrakennettu, ja sitä voidaan käsitellä kuten muitakin tulostevirtoja.

## See Also
- Järjestelmävirtojen käsittelyn perusteet: [geeksforgeeks.org](https://www.geeksforgeeks.org/system-out-println-in-java/)
- Virran ohjaaminen Unix-pohjaisissa järjestelmissä: [tldp.org](https://tldp.org/LDP/abs/html/io-redirection.html)
