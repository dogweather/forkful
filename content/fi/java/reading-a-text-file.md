---
title:                "Tiedoston lukeminen"
html_title:           "Java: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Miksi

Miksi kukaan haluaisi lukea tekstitiedostoa ohjelmoinnissa? Yleisin syy on tiedon lukeminen, kuten käyttäjän antamat asetukset tai tallennetut tiedot.

# Miten

Jos haluat lukea tekstitiedoston Java-ohjelmassa, voit käyttää File-luokkaa ja Scanner-luokkaa. Ensimmäinen luo yhteyden tiedostoon ja toinen lukee sen sisällön.

```Java
import java.io.File;
import java.util.Scanner;

public class Main {
  public static void main(String[] args) {
    try {
      // Luo uusi tiedosto-olio ja anna sen polku
      File tiedosto = new File("tiedosto.txt");
      // Luo uusi Scanner-olio lukemaan tiedosto
      Scanner lukija = new Scanner(tiedosto);
      // Luetaan tiedoston rivejä
      while (lukija.hasNextLine()) {
        String rivi = lukija.nextLine();
        System.out.println(rivi); // Tulostetaan rivi konsoliin
      }
      // Muista sulkea lukija
      lukija.close();
    } catch (Exception e) { // Jos tapahtuu virheitä
        System.out.println("Tiedoston lukeminen epäonnistui.");
        e.printStackTrace();
    }
  }
}
```

Esimerkkitiedoston "tiedosto.txt" sisältö voisi olla:

```
Tervetuloa ohjelmointimaailmaan!
Opi Javaa helposti ja nopeasti.
Ole hyvä ja kirjoita Feedback osoitteeseemme.
```

Tulostus konsolissa:

```
Tervetuloa ohjelmointimaailmaan!
Opi Javaa helposti ja nopeasti.
Ole hyvä ja kirjoita Feedback osoitteeseemme.
```

# Syvempi sukellus

Tekstitiedoston lukeminen on tärkeä taito jokaiselle ohjelmoijalle. On hyödyllistä ymmärtää, miten tiedostoja voidaan käsitellä ja lukea Java-ohjelmassa. Tämä antaa mahdollisuuden tallentaa ja lukea käyttäjän antamia tietoja ja käsitellä niitä eri tavoin.

# Katso myös

- [Java File-luokka - JavaDoc](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Java Scanner-luokka - JavaDoc](https://docs.oracle.com/javase/7/docs/api/java/util/Scanner.html)