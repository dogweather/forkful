---
title:                "Tekstitiedoston lukeminen"
html_title:           "Java: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Lukemaan tekstitiedostoa tarkoittaa tietojen saamista tiedostosta ja niiden käsittelyä ohjelmassa. Ohjelmoijat tekevät tätä usein tarvittavien tietojen saamiseksi ja niiden käyttämiseksi ohjelmassa.

## Miten:

```Java
import java.io.File; // tarvitaan tiedoston käsittelyyn
import java.util.Scanner; // käytetty tiedoston lukemiseen

public class Tekstitiedosto {
    public static void main(String[] args) throws Exception { 
        File tiedosto = new File("tekstitiedosto.txt"); // luodaan tiedosto-olio
        Scanner lukija = new Scanner(tiedosto); // luodaan lukija-olio
        while(lukija.hasNextLine()) { // käydään läpi tiedoston rivit
            String rivi = lukija.nextLine(); // tallennetaan rivi muuttujaan
            System.out.println(rivi); // tulostetaan rivi
        }
        lukija.close(); // suljetaan lukija
    }
}
```
Esimerkki tiedostossa olevasta tekstistä ja koodin tulostamasta:

Tiedosto sisältää seuraavan tekstin:
```
Tämä on esimerkkirivi.
Käytä tätä tiedostoa opetellaksesi tekstitiedoston lukemista.
```

Koodin tulostama tulos:
```
Tämä on esimerkkirivi.
Käytä tätä tiedostoa opetellaksesi tekstitiedoston lukemista.
```

## Syvällinen sukellus:

Tekstitiedostojen lukeminen on ollut tärkeä osa ohjelmointia jo pitkään. Se mahdollistaa tietojen saamisen ja hyödyntämisen ohjelmassa ilman, että niitä täytyy kovakoodata suoraan ohjelmaan. Tämä tekee ohjelmista joustavampia ja helpommin muokattavia.

Vaihtoehtoisia tapoja lukea tekstitiedostoja ovat esimerkiksi BufferedReader ja FileReader-luokat. Näitä voi käyttää myös tiedoston lukemiseen, mutta Scanner-luokan käyttäminen on yleensä helpompaa ja selkeämpää.

Tekstitiedostojen lukeminen voi sisältää myös tietojen erottelemista (parsing) ja käsittelyä. Tämä voi vaatia enemmän koodia ja erikoistoimintoja, mutta se antaa enemmän mahdollisuuksia tietojen käsittelyyn ja hyödyntämiseen.

## Katso myös:

Lue lisää tekstitiedostojen lukemisesta: https://www.tutorialspoint.com/java/java_files_io.htm

Tutustu Scanner-luokkaan: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Scanner.html