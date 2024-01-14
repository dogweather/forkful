---
title:    "Java: Tekstitiedoston lukeminen"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Monilla Java-ohjelmoijilla on tarve lukea tekstitiedostoja. Tämä voi olla osa tiedonkeruuprosessia tai tiedostonkirjoitusjärjestelmän osa. Riippumatta syystä, on tärkeää tuntea tekstitiedostojen lukemisen prosessi Java-ohjelmoinnissa. Tässä blogikirjoituksessa keskitymme siihen, miten voit lukea tekstitiedostoja Java-ohjelmassa.

## Miten

Java-ohjelmassa voidaan lukea tekstitiedostoja monin eri tavoin, mutta yksi näppärä tapa on käyttää Scanner-luokkaa. Se tarjoaa helpon tavan lukea tiedostoja ja tiedoston sisältöä voi käsitellä helposti.

Aloitamme luomalla Scanner-olion ja liitämme siihen tiedoston käyttämällä koodia:

```Java
Scanner lukija = new Scanner(new File(tiedostonimi));
```
Tämän jälkeen voimme käyttää lukija-oliota lukemaan tiedoston sisältöä käyttämällä metodia ```nextLine()```, joka lukee yhden rivin kerrallaan. Voimme myös käyttää muita metodeja, kuten ```nextInt()``` tai ```nextDouble()``` lukeaksemme erilaisia tietotyyppejä sisältäviä tiedostoja.

Jokaisen tiedoston lopussa tulee muistaa sulkea Scanner-olio ```close()```-metodilla, jotta varmistetaan, että resurssit vapautetaan oikein.

Näytetään esimerkki siitä, miten voit lukea ja tulostaa tekstitiedoston sisältöä Java-koodissa:

```Java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class TiedostonLuku {
  public static void main(String[] args) throws FileNotFoundException {
    // Luodaan Scanner-olio lukemaan tekstitiedostoa
    Scanner lukija = new Scanner(new File("tiedosto.txt"));

    // Luetaan ja tulostetaan tiedoston sisältöä rivi kerrallaan
    while (lukija.hasNextLine()) {
      String rivi = lukija.nextLine();
      System.out.println(rivi);
    }

    // Suljetaan Scanner-olio lopuksi
    lukija.close();
  }
}
```

Esimerkkitiedoston "tiedosto.txt" sisältö voi olla esimerkiksi:

```
Tämä on ensimmäinen rivi
Toinen rivi
Viimeinen rivi
```

Tulostuksen pitäisi näyttää seuraavalta:

```
Tämä on ensimmäinen rivi
Toinen rivi
Viimeinen rivi
```

## Syvempi sukellus

Scanner-luokka tarjoaa myös muita hyödyllisiä metodeja, jotka voit avulla voit käsitellä tiedoston sisältöä tehokkaasti. Voit esimerkiksi käyttää metodeja kuten ```hasNext()``` ja ```next()``` lukiessasi tiedoston eri tietotyyppejä. Lisäksi voit myös määrittää erilaisia erotimia, kuten välilyönnin tai rivinvaihdon, käyttämällä metodia ```useDelimiter()```.

Lisätietoja Scanner-luokasta ja sen tarjoamista metodeista löydät Java:n virallisesta dokumentaatiosta.

## Katso myös

- [Java Scanner-luokan dokumentaatio](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Scanner.html)
- [Lue tekstitiedosto Java Scanner-luokalla - opetusvideo (englanniksi)](https://www.youtube.com/watch?v=HBydRw1yezQ)