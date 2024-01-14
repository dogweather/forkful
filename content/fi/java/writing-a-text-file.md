---
title:                "Java: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Miksi

Java on yksi maailman suosituimmista ohjelmointikielistä, ja sen avulla voidaan luoda monenlaisia sovelluksia. Yksi tärkeä osa ohjelmointia on tiedostonhallinta, esimerkiksi tekstitiedostojen kirjoittaminen. Tässä blogitekstissä opit, miksi on tärkeää osata kirjoittaa tekstitiedostoja ja kuinka se tapahtuu Javalla.

# Kuinka

Tekstitiedoston kirjoittaminen Javalla on varsin yksinkertaista. Se voidaan tehdä muutamalla rivillä koodia. Alla on esimerkki, jossa luodaan uusi tekstitiedosto nimeltä "uusi_tiedosto.txt" ja kirjoitetaan siihen "Hei maailma!".

```Java
import java.io.File;
import java.io.FileWriter;

public class TekstitiedostonKirjoittaminen {

    public static void main(String[] args) {

        // Luodaan uusi tiedosto
        File tiedosto = new File("uusi_tiedosto.txt");

        try {
            // Avataan tiedoston kirjoittaja
            FileWriter kirjoittaja = new FileWriter(tiedosto);
            // Kirjoitetaan haluttu teksti
            kirjoittaja.write("Hei maailma!");
            // Suljetaan kirjoittaja
            kirjoittaja.close();
            // Ilmoitetaan käyttäjälle, että tiedosto on kirjoitettu onnistuneesti
            System.out.println("Tekstitiedostoon on kirjoitettu teksti.");
        } catch (Exception e) {
            System.out.println("Tiedoston kirjoittaminen epäonnistui.");
            e.printStackTrace();
        }
    }
}
```

Suoritaessa ohjelman, uusi_tiedosto.txt -niminen tiedosto luodaan automaattisesti samaan kansioon, missä koodi sijaitsee. Jos haluat kirjoittaa tiedostoon enemmän kuin yhden rivin tekstiä, voit käyttää metodia ```write```, joka ottaa parametrinaan String-tyyppisen muuttujan.

# Syvemmälle

Tekstitiedostojen kirjoittamisen lisäksi Javalla on myös muita vaihtoehtoja tiedostonhallintaan, kuten tiedostojen lukeminen ja niiden sisällön muokkaaminen. Voit myös kirjoittaa tekstiä suoraan olemassa olevaan tiedostoon sen sijaan, että luot uuden tiedoston. Tiedostonhallintaa tutkiessa kannattaa tutustua myös tiedostonhallintaluokkiin, kuten ```FileReader``` ja ```BufferedReader```, jotka tarjoavat enemmän toiminnallisuutta tiedostojen käsittelyyn.

# Katso myös

- Java-tiedostonhallinta - dokumentaatio: https://docs.oracle.com/javase/tutorial/essential/io/file.html
- Java-text - Esimerkkejä tiedostojen lukemisesta ja kirjoittamisesta: https://www.programiz.com/java-programming/examples/write-text-file
- Tiedostonhallintaluokat Javassa: https://www.geeksforgeeks.org/file-handling-java-using-filewriter-filereader/