---
title:    "Java: Komentoriviparametrien lukeminen"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Miksi lukea komenoriviparametreja Java-ohjelmoinnissa?

Komenoriviparametrien lukeminen on tärkeä taito Java-ohjelmointia opetellessa, sillä se mahdollistaa interaktiivisemman käyttöliittymän ja monipuolisemman ohjelmoinnin. Lisäksi se on olennainen osa monia Java-ohjelmia, kuten käyttöjärjestelmätyökaluja ja verkkopalveluita. Siksi on tärkeää hallita tämä taito, jotta voi kehittää monipuolisempia ohjelmia ja ymmärtää jo olemassa olevia ohjelmia.

## Kuinka lukea komenoriviparametreja Java-ohjelmassa?

Komenoriviparametrit luetaan ohjelmassa käyttämällä `args`-muuttujaa. Tämä muuttuja sisältää taulukon kaikista syötetyistä parametreistä, ja se voidaan iteroida läpi esimerkiksi `for`-silmukan avulla. Alla on esimerkki koodista, joka tulostaa kaikki syötteet ja niiden indeksin.

```Java
public class ArgsExample {
  public static void main(String[] args) {
    for (int i = 0; i < args.length; i++) {
      System.out.println("Syöte " + i + ": " + args[i]);
    }
  }
}
```

Jos ajetaan ohjelma komentoriviltä esimerkiksi seuraavalla tavalla:

```
java ArgsExample hello world
```

Tulostuu seuraava tulos:

```
Syöte 0: hello
Syöte 1: world
```

## Syvempi sukellus komenoriviparametreihin

Komenoriviparametrien lukeminen on mahdollista myös erilaisilla kirjastoilla, kuten Apache Commons CLI:llä. Tämän avulla voi tarkemmin määritellä odotetut parametrit ja niiden muodon. Lisäksi komenoriviparametreilla voidaan myös ohjata ohjelman toimintaa, esimerkiksi määrittelemällä erilaisia vaihtoehtoja. Tämän vuoksi on hyvä tutustua erilaisiin ratkaisuihin ja valita niistä itselleen sopivin.

## Katso myös

- [Java-varausavainsanat](https://docs.oracle.com/javase/tutorial/java/nutsandbolts/index.html)
- [Apache Commons CLI](https://commons.apache.org/proper/commons-cli/)
- [Komentoriviparametrien käyttö Java-ohjelmassa](https://www.baeldung.com/java-command-line-arguments)