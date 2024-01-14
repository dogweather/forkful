---
title:                "Java: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

### Miksi: Miksi luoda väliaikainen tiedosto Java-ohjelmoinnissa?

Väliaikaiset tiedostot ovat hyödyllisiä monissa ohjelmoinnin tilanteissa. Niitä käytetään yleensä silloin, kun tarvitaan väliaikaista tallennuspaikkaa tiedoille, kuten väliaikaiselle käyttäjäsessionille tai väliaikaiselle tietokannalle.

### Kuinka tehdä se: Esimerkkejä ja tulosteita Java-koodilohkoissa

```Java
try {
    // Luodaan väliaikainen tiedosto käyttäen Javan valmista luokkaa
    File tempFile = File.createTempFile("temporary", ".txt");
    System.out.println("Luotu väliaikainen tiedosto: " + tempFile.getAbsolutePath());
    
    // Kirjoitetaan tiedostoon tekstiä
    PrintWriter writer = new PrintWriter(tempFile);
    writer.println("Tämä on väliaikainen tiedosto");
    writer.close();
    
    // Luetaan tiedoston sisältö ja tulostetaan se
    Scanner scanner = new Scanner(tempFile);
    System.out.println("Tiedoston sisältö: " + scanner.nextLine());
    scanner.close();
    
    // Poistetaan väliaikainen tiedosto
    tempFile.delete();
    System.out.println("Väliaikainen tiedosto poistettu.");
} catch (IOException e) {
    e.printStackTrace();
}
```

**Tuloste:**

```
Luotu väliaikainen tiedosto: /tmp/temporary6814996582689941970.txt
Tiedoston sisältö: Tämä on väliaikainen tiedosto
Väliaikainen tiedosto poistettu.
```

### Syvällinen tarkastelu: Tietoa väliaikaisten tiedostojen luomisesta

Java tarjoaa valmiin luokan File.createTempFile() väliaikaisen tiedoston luomiseen. Tämä metodi luo tiedoston, jonka nimi alkaa annetulla etuliitteellä ja päättyy haluttuun tiedostopäätteeseen. Tiedosto tallennetaan oletusarvoisesti väliaikaiseen hakemistoon, mutta sen sijaintia voidaan myös muuttaa antamalla toinen polku parametrina.

On tärkeää muistaa, että väliaikainen tiedosto ei ole sama asia kuin väliaikainen tiedostojärjestelmä. Tiedosto ei häviä automaattisesti, vaan se on poistettava manuaalisesti. Tämä voidaan tehdä käyttämällä File-luokan delete() -metodia.

### Katso myös:

- [Oracle Java-tiedostonhallintaopas](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Java-tiedostonhallintaluokat](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java-Scanner-luokka](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)
- [Java-koodin esimerkkitiedosto](https://github.com/arikaha/java-example-file)