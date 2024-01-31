---
title:                "Onko hakemisto olemassa? Tarkistaminen"
date:                  2024-01-20T14:56:41.186024-07:00
simple_title:         "Onko hakemisto olemassa? Tarkistaminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Tarkistaaksemme olemassaoloa, tutkimme, onko kohdekansio fyysisesti olemassa tiedostojärjestelmässä. Ohjelmoijat tekevät tämän välttääkseen virheitä tai poikkeuksia, kun yrittävät lukea, kirjoittaa tai muuten käsitellä kansioita, joita ei ehkä ole.

## How to: (Kuinka tehdä:)
```java
import java.nio.file.*;

public class DirectoryExists {
    public static void main(String[] args) {
        Path directoryPath = Paths.get("/path/to/directory");
        
        if (Files.exists(directoryPath)) {
            System.out.println("Kansio on olemassa!");
        } else {
            System.out.println("Kansiota ei löydy.");
        }
    }
}
```

Sample Output:
```
Kansio on olemassa!
```

tai 

```
Kansiota ei löydy.
```

## Deep Dive (Syväsukellus)

Aikaisemmin, ennen `java.nio.file` -pakettia, `File`-luokkaa käytettiin tiedostojen olemassaolon tarkistamiseen. `java.nio.file` esiteltiin Java 7:ssä ja toi mukanaan `Path`- ja `Files`-luokat, tarjoamalla paremman API:n tiedostojen käsittelyyn. Tarkistamiseen voi käyttää myös `Files.notExists(directoryPath)` metodia, mikä voi olla selkeämpi tietyissä konteksteissa. Pohdittaessa suorituskykyä, `Files.exists` tarkistaa tiedoston olemassaolon lukematta metatietoja, mikä tekee siitä nopeamman vaihtoehdon.

## See Also (Katso myös)

- [Official Java Documentation for `Files.exists`](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html#exists(java.nio.file.Path,java.nio.file.LinkOption...))
