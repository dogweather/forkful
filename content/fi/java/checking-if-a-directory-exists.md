---
title:                "Java: Tarkista onko hakemisto olemassa"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi: Tarkistaa, onko hakemistoa olemassa

On monia syitä, miksi Java-ohjelmoijat saattavat tarvita tarkistaa, onko hakemistoa olemassa. Näihin voi kuulua esimerkiksi tiettyjen tiedostojen etsiminen, varmistaminen että tiedostot tallennetaan oikeaan paikkaan tai yleisesti ottaen sovelluksen toiminnan hallinta. Tässä blogikirjoituksessa tutustumme siihen, kuinka voit tarkistaa hakemiston olemassaolon Java-ohjelmassasi.

## Kuinka tehdä se: Koodi-esimerkkejä ja -tulosteita

Java tarjoaa sisäänrakennetun tavan tarkistaa, onko hakemistoa olemassa. Tämä tehdään File-luokan avulla, joka vastaanottaa hakemiston polkunimen parametrina. Jos hakemisto on olemassa, File.exists() metodi palauttaa boolean-arvon true. Muussa tapauksessa se palauttaa arvon false.

```
Java

public class Main {
  public static void main(String[] args) {
    // Tarkistaa, onko hakemisto olemassa
    File directory = new File("polkunimi");
    boolean exists = directory.exists();
    System.out.println("Hakemisto on olemassa: " + exists);
  }
}
```

Esimerkkikoodin tulostus riippuu hakemiston olemassaolosta. Jos hakemisto on olemassa, tuloste on "Hakemisto on olemassa: true". Muussa tapauksessa tuloste on "Hakemisto on olemassa: false". Tämä yksinkertainen metodi on erittäin hyödyllinen esimerkiksi hakemistojen luomisessa tai tiettyjen tiedostojen etsimisessä tiettyyn paikkaan tallentamista varten.

## Syvempi sukellus: Tarkempia tietoja hakemiston olemassaolosta

On tärkeää huomata, että File.exists() -metodi tarkistaa vain, onko tiedostopolku olemassa. Se ei tarkista, onko kyseessä todellinen kansio vai tiedosto. Tämä tarkoittaa sitä, että File-luokka voi myös hyväksyä tiedostopolun, joka osoittaa tiedostoon, eikä hakemistoon. Niinpä on hyvä käytäntö tarkistaa myös tiedoston tyyppi, ennen kuin käytetään File.exists() -metodia.

Lisäksi on huomioitava, että File.exists() -metodin palauttama boolean-arvo ei kerro, onko kyseessä oleva polku todella tiedosto tai kansio. Se vain osoittaa, onko polku käytettävissä. Useimmissa tapauksissa tämä on kuitenkin tarpeeksi ja File.exists() -metodia käytetäänkin yleisesti hakemistojen ja tiedostojen olemassaolon tarkistamisessa.

## Katso myös

- Java File-luokka: https://docs.oracle.com/javase/8/docs/api/java/io/File.html
- File.exists() metodi: https://docs.oracle.com/javase/8/docs/api/java/io/File.html#exists--