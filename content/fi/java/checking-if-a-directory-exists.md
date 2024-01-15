---
title:                "Tarkista onko hakemisto olemassa"
html_title:           "Java: Tarkista onko hakemisto olemassa"
simple_title:         "Tarkista onko hakemisto olemassa"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

On hyödyllistä tarkistaa, onko hakemistoa olemassa, kun halutaan varmistaa, että tiettyä tiedostopolkua voidaan käyttää oikein ohjelmassa. Tämä auttaa myös välttämään virheitä ja etukäteen käsittelemään mahdollisia ongelmia.

## Miten

```java
if (directory.exists()) {
  System.out.println("Hakemisto on olemassa!");
} else {
  System.out.println("Hakemistoa ei ole olemassa.");
}
```

Koodin ensimmäisellä rivillä tarkistetaan, onko hakemisto olemassa ja sen jälkeen tulostetaan vastaava viesti riippuen tuloksesta. Java tarjoaa valmiin exists() -metodin, joka palauttaa totuusarvon sen perusteella, löytyykö hakemisto.

## Syvä Sukellus

Hakemiston olemassaolon tarkistaminen voi myös auttaa välttämään virheitä, kuten NullPointerExceptionia. Tämä on erityisen tärkeää silloin, kun käsitellään tiedostoja ja polkuja, joita käyttäjä voi itse muokata. Jos halutaan tarkistaa, että hakemisto on olemassa ja sitä voidaan käyttää, kannattaa käyttää myös try-catch -lohkoa.

```java
try {
  File directory = new File("polku/hakemisto");
  if (directory.exists()) {
    // tee jotain tiedostolle
  } else {
    // tulosta virheilmoitus tai tee jotain toista
  }
} catch (Exception e) {
  // käsittely virheille
}
```

Java tarjoaa myös muita hyödyllisiä metodeja ja ominaisuuksia, kuten isDirectory(), jonka avulla voidaan tarkistaa onko kyseessä hakemisto vai tiedosto.

## Katso myös

- [Java File-luokka](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Java File Systems -opas](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)
- [Java try-catch -lohko](https://docs.oracle.com/javase/tutorial/essential/exceptions/handling.html)