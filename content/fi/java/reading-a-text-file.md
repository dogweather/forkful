---
title:                "Java: Tekstitiedoston lukeminen"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Miksi lukea teksti tiedostoja Java-ohjelmoinnin yhteydessä?

Teksti tiedostot ovat yleinen tapa tallentaa ja jakaa tietoa ohjelmoinnin maailmassa. Ne voivat sisältää tekstejä, lukuarvoja, merkkejä ja jopa muita tiedostoja. Java-ohjelmoijana, sinun on tärkeää olla tietoinen siitä, miten lukea teksti tiedostoja ja käsitellä niiden sisältöä koodissa.

## Näin teet sen

Java-ohjelmointikielessä on sisäänrakennettuja toimintoja, joiden avulla voit lukea ja käsitellä teksti tiedostoja. Alla on esimerkki koodista, kuinka voit lukea tiedoston nimeltä "teksti.txt".

```Java
// Tuo tarvittavat kirjastot
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

// Luo uusi tiedosto-olio ja avaa se lukemista varten
File tiedosto = new File("teksti.txt");
FileReader lukija = new FileReader(tiedosto);

// Luetaan kaikki teksti ja tallennetaan se javan omalla "char" tyyppisellä muuttujalla
int merkki;
while ((merkki = lukija.read()) != -1) {
  char c = (char)merkki; // Muuta "int" tyyppinen merkki "char" tyyppiseksi
  // Tee halutut toimenpiteet luetulle merkille
}

// Sulje tiedosto lukemisen jälkeen
lukija.close();
```

Tämä koodi esimerkki luo ensin tarvittavat tulot (input) toiminnot ja tiedostoon viittaavat oliot. Sitten se lukee tiedoston sisällön merkki kerrallaan, tallentaa sen "char" muuttujaan ja suorittaa halutut toimenpiteet luetulle merkille. Lopuksi tiedosto suljetaan lukemisen jälkeen.

## Syvällisempi tarkastelu

On tärkeää muistaa, että teksti tiedostot voivat sisältää monenlaisia merkkejä ja niiden lukeminen Java-ohjelmassa vaatii oikeanlaisen koodauksen. Tämä tarkoittaa, että sinun tulee huolehtia siitä, että tiedosto tallennetaan ja luetaan samassa koodauksessa. Voit myös tarvittaessa asettaa koodauksen manuaalisesti luku- tai kirjoitusprosessin yhteydessä.

## Katso myös

- [Java-opas - Tiedostojen käsittely](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Codecademy - Java tiedostojen lukeminen ja kirjoittaminen](https://www.codecademy.com/learn/learn-java/modules/learn-java-files)
- [Baeldung - Tiedostojen lukeminen ja kirjoittaminen Javassa](https://www.baeldung.com/java-read-file)