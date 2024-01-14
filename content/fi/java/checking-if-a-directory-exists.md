---
title:    "Java: Tarkistetaan onko hakemisto olemassa"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi
Kansio on tärkeä osa tietokoneen tiedostorakennetta ja sen on oltava luotettava ja turvallinen. Tämän vuoksi on tärkeää tarkistaa, onko haluttu kansio jo olemassa, ennen kuin suoritetaan tähän kansioon liittyviä toimintoja.

## Kuinka tehdä
```Java
import java.io.File;

public class DirectoryCheck {
    public static void main(String[] args) {
        String directoryName = "Kansio";
        
        // luodaan uusi File-olio annetulla kansiotiedoston nimellä
        File directory = new File(directoryName);
                
        // tarkistetaan, onko kansio olemassa
        if (directory.exists()) {
            System.out.println("Kansio " + directoryName + " löytyy.");
        } else {
            System.out.println("Kansio " + directoryName + " ei löydy.");
        }
    }
}
```
Tässä esimerkissä käytetään File-luokkaa ja sen exist-metodia tarkistamaan, onko annettu kansio olemassa. Jos kansio löytyy, tulostetaan viesti, muussa tapauksessa tulostetaan virheilmoitus.

Esimerkkituloste:
```
Kansio löytyy.
```

## Syvemmälle aiheeseen
On tärkeää huomata, että exist-metodi ei tarkista vain kansiotiedostoa, vaan myös sen sisältöä. Jos kansio on olemassa, mutta tyhjä, exist-metodi palauttaa arvon 'false'.

Kansio voi myös olla piilossa, eli sen nimi alkaa pisteellä. Tällöin exist-metodi ei tunnista sitä, vaikka kansio olisikin olemassa.

On myös hyvä huomioida, että exist-metodi tarkistaa vain annetulla tiedostonimellä olevan kansion, eikä alikansioita. Jos haluat tarkistaa kaikki kansiotietokoneesta, voi olla tarpeen käyttää muita tiedostonhallintametodeja.

## Katso myös
- [File-luokan JavaDoc](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java.io-pakettidokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/io/package-summary.html)
- [Java-tiedostonhallinta Stack Overflowssa (englanniksi)](https://stackoverflow.com/questions/972192/how-can-i-get-the-list-of-files-in-a-directory-using-java)

### HUOM! Muista käsitellä tiedostoja ja kansioita turvallisesti ja vastuullisesti. Muista myös käyttää asianmukaisia tiedostonhallintametodeja ja varmistaa, että käyttäjällä on tarvittavat oikeudet tiedoston käsittelyyn.