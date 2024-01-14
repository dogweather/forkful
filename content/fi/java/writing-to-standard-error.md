---
title:                "Java: Kirjoittaminen standardivirheeseen"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Miksi?

Jokainen Java-ohjelmoija on varmasti törmännyt virheilmoituksiin. Pahimmillaan ne voivat hidastaa työskentelyä ja aiheuttaa harmaita hiuksia. Mutta mitä jos kerrommekin Java-ohjelmalle, että se näyttäisikin virheilmoituksensa standardi virhe -tulostevirrasta? Tämä voi tehdä ohjelmoinnista huomattavasti helpompaa ja mahdollistaa kätevämmän virheidenhallinnan.

# Kuinka?

Käyttämällä `System.err` -oliota voimme kirjoittaa virheilmoituksia suoraan standardi virhe -tulostevirtaan. Tämä tapahtuu juuri kuten standardi tulostevirtaan kirjoittaminen, mutta käytämme `println()`-metodia. Alla on esimerkki koodista ja tulos.

```Java
public class StandardErrorExample {
  public static void main(String[] args) {
    System.err.println("Tämä on virheilmoitus!");
  }
}
```

Tulos:

```
Tämä on virheilmoitus!
```

# Syvällisempi sukellus

Standardi virhe -tulostevirta on osa Java-kielen "systeemiviestintää". Sitä käytetään normaalisti näyttämään virheilmoituksia ohjelman suorituksen aikana. Tämä tapahtuu, kun esimerkiksi koodissa tapahtuu jokin virhe tai poikkeus. Voimme siis ajatella standardi virhe -tulostevirtaa vähän kuin ohjelman huudoille.

Virheilmoitusten lisäksi voimme käyttää standardi virhe -tulostevirtaa myös muuhun viestintään. Esimerkiksi jos haluamme näyttää käyttäjälle jonkin ilmoituksen, voimme kirjoittaa sen standardi virhe -tulostevirtaan ja se näkyy samalla tavalla kuin virheilmoitukset.

# Katso myös

- Java virheidenhallinta: https://docs.oracle.com/javase/tutorial/essential/exceptions/index.html
- System.err Java-kirjaston dokumentaatio: https://docs.oracle.com/javase/7/docs/api/java/lang/System.html#err
- Java virheet ja poikkeukset: https://www.javatpoint.com/exception-handling-in-java