---
title:    "Java: Lukeminen tekstitiedostosta"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Miksi

Lukemisen tarve on keskeinen osa Java-ohjelmointia, erityisesti tekstilukuihin. Tavallisesti pystymme lukemaan ja kirjoittamaan tekstejä konsoliin, mutta on myös tärkeää pystyä lukemaan tekstitiedostoja, joita ohjelma käsittelee. Tämä mahdollistaa ohjelmien vuorovaikutuksen ulkomaailman kanssa, mukaan lukien tietokannat ja käyttäjien syöttämät tiedot. Siksi on tärkeää osata lukea tekstitiedostoja Java-ohjelmoinnissa.

# Kuinka tehdä

Tekstitiedostojen lukeminen Java-ohjelmoinnissa on yksinkertaista, kun tiedämme mitä tehdä. Tässä on muutama esimerkki koodi ja tuloste, joiden avulla voit oppia lukemaan tekstitiedostoja Java-ohjelmointiympäristössä.

```Java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class TextFileReader {

   public static void main(String[] args) {

     // Määritellään tiedosto, joka halutaan lukea
     File file = new File("tiedostonimi.txt");

     try {
       // Luodaan Scanner-olio lukemaan tiedostoa
       Scanner scanner = new Scanner(file);

       // Luetaan tiedosto rivi riviltä ja tulostetaan konsoliin
       while (scanner.hasNextLine()) {
         String line = scanner.nextLine();
         System.out.println(line);
       }
       scanner.close();

     } catch (FileNotFoundException e) {
       System.out.println("Tiedostoa ei löydy.");
       e.printStackTrace();
     }
   }
}
```

Tuloste:

```
Tämä on tekstirivi 1
Tämä on tekstirivi 2
Tämä on tekstirivi 3
```

# Syvemmälle

Tekstitiedostojen lukeminen Java-ohjelmoinnissa tapahtuu usein käyttämällä Scanner-luokkaa, joka mahdollistaa tiedoston rivien lukemisen yksi kerrallaan. Scanner-luokka tarjoaa myös muita hyödyllisiä metodeja tiedoston lukemiseen, kuten .hasNext() ja .next(), jotka mahdollistavat erilaisten tietotyyppien, kuten int ja double, lukemisen tiedostosta.

On myös hyvä muistaa sulkea Scanner-olio käytön jälkeen, jotta se ei jää roikkumaan ohjelman muistiin. Tämä tapahtuu .close() metodilla.

Haluamme myös varmistaa, että tiedostoa ei löydy ennen sen lukemisen aloittamista, jotta vältymme ohjelman kaatumiselta. Tämä tehdään try-catch lohkon avulla, jossa mahdolliset ongelmat käsitellään try lohkossa ja virheet tulostetaan catch lohkossa.

# Katso myös

1. [JavaDoc: Scanner-luokka](https://docs.oracle.com/javase/7/docs/api/java/util/Scanner.html)
2. [Java: Tiedoston lukeminen - W3Schools](https://www.w3schools.com/java/java_files_read.asp)
3. [Tiedostojen lukeminen ja kirjoittaminen Java:lla - TutorialsPoint](https://www.tutorialspoint.com/java/io/java_io_bufferedreader.htm)