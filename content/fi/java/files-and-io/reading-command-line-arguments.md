---
title:                "Komennoriviparametrien lukeminen"
aliases:
- /fi/java/reading-command-line-arguments.md
date:                  2024-01-20T17:56:07.402580-07:00
model:                 gpt-4-1106-preview
simple_title:         "Komennoriviparametrien lukeminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
"Mitä & Miksi?"

Komennonriviargumentit ovat ohjelmaan ulkoa syötettäviä parametreja. Niitä käytetään ohjelman toiminnan muokkaamiseen lennossa, ilman koodin muuttamista.

## How to:
"Miten tehdään:"

```java
public class CommandLineExample {
    public static void main(String[] args) {
        if (args.length > 0) {
            System.out.println("Hei, tässä komennonriviargumenttisi:");
            for (String arg : args) {
                System.out.println(arg);
            }
        } else {
            System.out.println("Ei komennonriviargumentteja annettu.");
        }
    }
}
```

Komentoikkunassa ohjelman ajamisen yhteydessä:

```shell
java CommandLineExample Terve Maailma!
```

Tulostaa:

```
Hei, tässä komennonriviargumenttisi:
Terve
Maailma!
```

## Deep Dive
"Sukellus syvemmälle:"

Kun Java-ohjelma käynnistetään, `main`-metodin `args`-taulukko täytetään käyttäjän antamilla komennonriviargumenteilla. Historiallisesti tämä on ollut olennainen osa monia ohjelmia, koska sen avulla on voitu välittää asetuksia ja käyttäjätietoja ohjelman suorituksen aikana.

Vaihtoehtoina komennonriviargumenteille voidaan käyttää esimerkiksi konfiguraatiotiedostoja, ympäristömuuttujia tai interaktiivisia käyttöliittymiä, mutta argumentit tarjoavat yksinkertaisen ja nopean tavan ohjata ohjelmaa.

Java 5 toi `java.util.Scanner`in ja Java 8 `java.util.stream`in, joilla voidaan lukea syötteitä suoraa ohjelman sisällä, mutta komennonriviargumenttien merkitys pysyy erityisesti skriptimäisissä sovelluksissa, joissa tarvitaan nopeaa parametrisointia.

## See Also
"Katso myös:"

- Official Java Tutorials: [Command-Line Arguments](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- Stack Overflow: How to parse command-line arguments in Java? ([link](https://stackoverflow.com/questions/367706/how-to-parse-command-line-arguments-in-java))
