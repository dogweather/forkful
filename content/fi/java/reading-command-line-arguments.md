---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Java: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Mitä ja miksi?

Kun ohjelmoijat kehittävät Java-ohjelmia, heidän täytyy usein lukea syötteitä käyttäjältä tai ympäristöstä, jotta ohjelmat voivat toimia oikein. Yksi tapa tehdä tämä on lukemalla komentoriviparametreja, eli käyttäjän antamia tietoja ohjelman suorittamisen aikana.

Kuinka:

Java tarjoaa helpon tavan lukea komentoriviparametreja käyttäen `args`-muuttujaa. Alla on esimerkki koodista, joka tulostaa käyttäjän antaman syötteen komentoriviltä:

```Java
public class CommandLine {
    public static void main(String[] args) {
        System.out.println(args[0]); //tulostaa ensimmäisen komentoriviparametrin
    }
}
```

Jos käyttäjä antaa ohjelmalle seuraavan komennon: `java CommandLine Hello`, ohjelma tulostaa `Hello`.

Syötteiden lukeminen komentoriviltä on hyödyllistä silloin kun ohjelman toimintaan voidaan vaikuttaa käyttämällä erilaisia parametreja.

Syvempää tietoa:

Komentoriviparametrien käyttö on ollut osa Java-kieltä lähtien sen ensimmäisestä versiosta. Aluksi käytettiin `getSystemProperty()`-metodia, mutta sisäänrakennettu `args`-muuttuja tekee tästä helpompaa ja suorempaa. Java-tiedostojen lisäksi komentoriviparametreja voidaan lukea myös työkoneella ajettaessa, käyttäen `appletviewer`-työkalua.

Katso myös:

Lue lisää komentoriviparametrin lukemisesta ja sen käytöstä Java-tiedostoissa täältä: https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html