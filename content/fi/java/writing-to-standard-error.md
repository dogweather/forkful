---
title:                "Java: Kirjoittaminen standardi virheeseen"
simple_title:         "Kirjoittaminen standardi virheeseen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen standardidatalle voi olla erittäin hyödyllistä ohjelmoijille useista eri syistä. Erityisesti silloin, kun halutaan havainnollistaa ja seurata ohjelman suoritusta tai vikasignaaleja, standardidatan kirjoittaminen voi olla erittäin hyödyllistä. Se myös auttaa tunnistamaan ja korjaamaan mahdollisia ohjelmointivirheitä.

## Miten

**Esimerkkikoodi Java:**

```java
public static void main(String[] args) {
    System.err.println("Tämä näyttää viestin standardidatalla"); 
    System.out.println("Tämä näyttää viestin standardivirrassa");
}
```

**Esimerkkilähtö:**

```
Tämä näyttää viestin standardidatalla
Tämä näyttää viestin standardivirrassa
```

Kuten näkyy esimerkistä, ero standardidatan ja standardivirran välillä on pieni, mutta merkittävä. Käyttämällä syöttövirran sijaan virhevirran kirjoittamista, voit helposti seurata ohjelman suoritusta ja havaita mahdolliset ongelmat.

## Syvällinen tutkimus

Toinen tapa hyödyntää standardidataa ohjelmoinnissa on käyttää sitä väliaikaisena tallennustilana. Esimerkiksi jos sinun on tarkoitus käsitellä suuri määrä tietoa, voit ohjelmoida ohjelmasi kirjoittamaan tarvittavat tiedot standardidatalle ja lukea ne myöhemmin takaisin.

Yksi tärkeä asia, jonka sinun tulee muistaa kirjoittaessasi standardidatalle on käsitellä kaikki mahdolliset poikkeukset (exceptions). Jos virhe ilmenee, ohjelma saattaa kaatua tai olla mahdoton palauttaa tiedot. Parhaan tuloksen saavuttamiseksi kannattaa hyödyntää poikkeusten käsittelyä käyttäen esimerkiksi try-catch -lohkoja.

## Katso myös

Lisätietoa standardidatan käytöstä Java-ohjelmoinnissa löydät seuraavista lähteistä:

- https://www.geeksforgeeks.org/standard-output-streams-in-java/
- https://docs.oracle.com/javase/7/docs/api/java/lang/System.html
- https://www.baeldung.com/java-system-out-println-vs-system-err-println