---
title:                "Kirjoittaminen standardivirheeseen"
html_title:           "Java: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Kirjoittaminen standardierroriin on tapa tulostaa virheitä ja antaa tärkeitä tietoja koodin suorituksen aikana. Tämä auttaa ohjelmoijaa korjaamaan ja muokkaamaan koodiaan tehokkaasti.

## Kuinka tehdä se:
```Java
System.err.println("Virheilmoitus tulee tähän"); 
```

Esimerkiksi, jos haluat tulostaa virheilmoituksen, että luku ei voi olla negatiivinen, voit käyttää seuraavaa koodia:

```Java
if (luku < 0) {
    System.err.println("Luku ei voi olla negatiivinen!");
}
```

Tämä tulostaisi virheilmoituksen konsoliin ja auttaisi sinua tunnistamaan ja korjaamaan ongelman.

## Syvällinen sukellus:
Kirjoittaminen standardierroriin on osa ohjelman suoritusympäristöä, joka on vastuussa virheiden ilmoittamisesta. Tämä tapa eroaa standarditulostuksesta, joka näytetään konsolissa. On myös muita tapoja ilmoittaa virheitä, kuten käyttämällä loggausta tai poikkeuksia.

## Katso myös:
- [Java System-luokka] (https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- [Java Development Kit (JDK)] (https://www.oracle.com/java/technologies/javase-jdk15-downloads.html)