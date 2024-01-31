---
title:                "Tekstitiedoston kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"

category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Tekstitiedoston kirjoittaminen tarkoittaa tiedon tallentamista tekstimuodossa tiedostoon. Ohjelmoijat kirjoittavat tiedostoja, koska se on helppo tapa tallentaa ja jakaa tietoa pysyvästi.

## How to:
Tässä on yksinkertainen esimerkki tekstitiedoston kirjoittamisesta Javalla:

```Java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriter {
    public static void main(String[] args) {
        String text = "Hei! Tämä on tekstiviesti tiedostoon.";
        String filePath = "tervehdys.txt";

        try (BufferedWriter writer = new BufferedWriter(new FileWriter(filePath))) {
            writer.write(text);
            System.out.println("Tiedosto kirjoitettu: " + filePath);
        } catch (IOException e) {
            System.out.println("Virhe tiedostoa kirjoittaessa: " + e.getMessage());
        }
    }
}
```
Tämä koodi luo "tervehdys.txt" nimisen tiedoston ja kirjoittaa siihen "Hei! Tämä on tekstiviesti tiedostoon.".

## Deep Dive
Tekstitiedoston kirjoittamisen historia ulottuu tietokoneiden alkuaikoihin, jolloin tiedot tallennettiin magneettinauhalle. Nykyään on monia tapoja kirjoittaa tekstitiedostoja Javassa, kuten `FileWriter`, `BufferedWriter`, `PrintWriter` ja `Files` luokat. `BufferedWriter` on suosittu, koska se tarjoaa puskuroinnin, mikä parantaa suorituskykyä suurten tiedostojen kirjoittamisessa. Tiedostojen käsittelyyn liittyy kuitenkin aina virheiden käsittely – siksi virheenkäsittely (esim. try-with-resources) on olennainen osa tiedostojen kirjoittamista.

## See Also
- [Oracle's Official Java Documentation for FileWriter](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/io/FileWriter.html)
- [BufferedWriter documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/io/BufferedWriter.html)
- [Java Practices for Reading and Writing Files](http://www.javapractices.com/topic/TopicAction.do?Id=42)
- [Stack Overflow: Writing a file in Java](https://stackoverflow.com/questions/2885173/how-do-i-create-a-file-and-write-to-it-in-java)
