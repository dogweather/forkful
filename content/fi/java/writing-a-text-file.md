---
title:                "Tekstitiedoston kirjoittaminen"
aliases:
- fi/java/writing-a-text-file.md
date:                  2024-02-03T19:28:25.215622-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tekstitiedoston kirjoittaminen"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?

Tekstitiedoston kirjoittaminen Javalla liittyy kielen kyvykkyyksien hyödyntämiseen sisällön luomiseen ja kirjoittamiseen tiedostojärjestelmän tiedostoihin. Ohjelmoijat tekevät tätä monista syistä, kuten lokitiedostojen kirjaamiseen, datan viemiseen tai sovelluksen tilan tallentamiseen myöhempää noutoa varten.

## Kuinka:

### Käyttäen `java.nio.file` (Vakiokirjasto)

Javan uusi I/O (NIO)-paketti (`java.nio.file`) tarjoaa monipuolisemman lähestymistavan tiedostojen käsittelyyn. Tässä on yksinkertainen tapa kirjoittaa tiedostoon käyttäen `Files.write()`:

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class TextFileWriterNIO {
    public static void main(String[] args) {
        List<String> lines = Arrays.asList("Rivi 1", "Rivi 2", "Rivi 3");
        try {
            Files.write(Paths.get("example.txt"), lines);
            System.out.println("Tiedosto kirjoitettu onnistuneesti!");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Tuloste:

```
Tiedosto kirjoitettu onnistuneesti!
```

### Käyttäen `java.io` (Vakiokirjasto)

Perinteisempää lähestymistapaa varten `java.io.FileWriter` on hyvä valinta tekstiedostojen yksinkertaiseen kirjoittamiseen:

```java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriterIO {
    public static void main(String[] args) {
        try (FileWriter writer = new FileWriter("example.txt")) {
            writer.write("Hei, Maailma!\n");
            writer.append("Tämä on toinen rivi.");
            System.out.println("Tiedosto kirjoitettu onnistuneesti!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Tuloste:

```
Tiedosto kirjoitettu onnistuneesti!
```

### Käyttäen Apache Commons IO

Apache Commons IO -kirjasto yksinkertaistaa monia operaatioita, mukaan lukien tiedostoon kirjoittamisen. Näin kirjoitat tiedostoon käyttäen `FileUtils.writeStringToFile()`:

Lisää ensin riippuvuus projektiisi. Jos käytät Mavenia, sisällytä:

```xml
<dependency>
  <groupId>org.apache.commons</groupId>
  <artifactId>commons-io</artifactId>
  <version>2.11.0</version> <!-- Tarkista viimeisin versio -->
</dependency>
```

Käytä sitten seuraavaa koodia tekstin kirjoittamiseen tiedostoon:

```java
import org.apache.commons.io.FileUtils;
import java.io.File;
import java.io.IOException;

public class TextFileWriterCommonsIO {
    public static void main(String[] args) {
        try {
            FileUtils.writeStringToFile(new File("example.txt"), "Tämä on tekstiä, joka on kirjoitettu käyttäen Commons IO:ta.", "UTF-8");
            System.out.println("Tiedosto kirjoitettu onnistuneesti!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

```

Tuloste:

```
Tiedosto kirjoitettu onnistuneesti!
```
