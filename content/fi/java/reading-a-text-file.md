---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Java Ohjelmointi: Kuinka lukea tekstitiedosto?

#### ## Mitä & Miksi?
Tekstitiedoston lukeminen Java-ohjelmointikielessä tarkoittaa tiedoston sisällön hankkimista niin, että ohjelma voi sitä käsitellä. Tätä käytetään usein datan analysointiin, tiedon jakeluun ja moniin muihin tarkoituksiin, kuten verkkokehitys tai tietojenkäsittely.

#### ## Kuinka:
Alla on perusesimerkki siitä, miten voit lukea tekstitiedoston Java-ohjelmoinnissa.

```Java
import java.nio.file.*;

public class ReadFile {
    public static void main(String[] args) throws Exception {
        Path filePath = Paths.get("file.txt");
        String content = Files.readString(filePath);
        System.out.println(content);
    }
}
```
Kun suoritat tämän koodin ja `file.txt` sisältää esimerkiksi tekstin "Hei maailma", saadaan tulosteeksi:
```
Hei maailma
```
#### ## Syvälle menevä
Tekstitiedoston lukeminen on ollut keskeinen osa ohjelmointia sen alkuajoista lähtien ja se on peräisin C-ohjelmointikielen tehtävistä. Vaikka modernit tarpeet ovat johtaneet uusiin lähestymistapoihin ja työkaluihin, peruskonsepti on pysynyt samana.

Java tarjoaa myös vaihtoehtoisia tapoja tekstitiedostojen käsittelyyn, kuten `Scanner` luokan ja `BufferedReader` luokan. Kumpikin niistä on hyödyllinen erilaisissa tilanteissa, riippuen esimerkiksi tiedoston koosta ja käsittelyn monimutkaisuudesta.

Tekstitiedoston lukeminen voi vaatia käyttöjärjestelmän resursseja, mukaan lukien muistia ja suorittimia. On tärkeää huomioida nämä seikat, erityisesti suurten tiedostojen tai useiden tiedostojen samanaikaisen käsittelyn yhteydessä.

#### ## Katso myös
- Java API [Files.readString](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/nio/file/Files.html) dokumentaatio
- Oracle artikkeli: [Basic I/O](https://docs.oracle.com/javase/tutorial/essential/io/index.html)
- Stack Overflow keskustelu: [How to read a large text file line by line using Java?](https://stackoverflow.com/questions/5868369)