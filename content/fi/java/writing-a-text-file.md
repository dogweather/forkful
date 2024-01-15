---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Java: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedostojen kirjoittaminen on tärkeä osaohjelmointia, sillä se mahdollistaa tietojen tallentamisen ja jakamisen eri sovellusten välillä. Se on myös hyödyllistä silloin, kun halutaan tallentaa suuria datamääriä tai luoda mukautettuja käyttöliittymiä tiedostojen käsittelyyn.

## Miten

Tekstitiedostojen kirjoittaminen Java-kielellä on yksinkertaista ja sisältää useita vaiheita. Ensinnäkin, luodaan File-objekti haluttua tiedostoa varten ja luodaan FileWriter- ja BufferedWriter-objektit, jotka mahdollistavat tiedon kirjoittamisen tiedostoon. Seuraavaksi avataan tiedosto, kirjoitetaan haluttu teksti ja suljetaan tiedosto. Alla on esimerkki koodista ja sen generoima tuloste.

```Java
// Luodaan File-objekti
File file = new File("tekstitiedosto.txt");
 
// Luodaan FileWriter ja BufferedWriter
FileWriter fw = new FileWriter(file);
BufferedWriter bw = new BufferedWriter(fw);
 
// Avataan tiedosto
bw.write("Tervetuloa tekstitiedostojen maailmaan!");
bw.newLine();
bw.write("Tämä on Java-ohjelmoinnilla luotu tiedosto.");
bw.newLine();
 
// Suljetaan tiedosto
bw.close();
 
// Tulostetaan tiedoston sisältö
System.out.println("Tekstitiedoston sisältö:");
Scanner input = new Scanner(file);
while (input.hasNext()) {
    String line = input.nextLine();
    System.out.println(line);
}
input.close();
```

Tuloste:

```
Tekstitiedoston sisältö:
Tervetuloa tekstitiedostojen maailmaan!
Tämä on Java-ohjelmoinnilla luotu tiedosto.
```

## Syväsukellus

Tekstitiedostojen luominen Java-kielellä vaihtelee jonkin verran käytettyjen kirjastojen, kuten FileWriter, BufferedWriter ja Scanner, mukaan. Kirjoittaessa on tärkeää ottaa huomioon myös tiedostojen sijainti ja oikeudet käyttöjärjestelmässä. Seuraavassa vaiheessa ResourceBundle-luokkaa voidaan käyttää muuttujien poimimiseen tiedostosta, jolloin tekstin ei tarvitse olla kirjoitettuna suoraan koodiin.

## Katso myös

- [Java – Streaming File Writing Example](https://www.baeldung.com/java-write-to-file)
- [Java - FileWriter Class](https://www.tutorialspoint.com/java/io/filewriter_write_charsequence_start_offset_len-method.htm)
- [Java – ResourceBundle Tutorial](https://www.codejava.net/java-core/java-util-resourcebundle-class-tutorial-and-examples)