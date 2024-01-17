---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "Go: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Väliaikaisten tiedostojen luominen on yleinen käytäntö ohjelmoinnissa. Se tarkoittaa tilapäisten tiedostojen luomista, joita käytetään väliaikaisesti ohjelman suorituksen aikana. Tämä on hyödyllistä esimerkiksi silloin, kun tarvitaan väliaikaista tallennustilaa tai kun tiedostoa ei haluta tallentaa pysyvästi.

## Miten:
Voit luoda väliaikaisen tiedoston käyttämällä Go-ohjelmointikielen tarjoamaa ```ioutil.TempFile()```-funktiota. Tämä luo uuden tiedoston ja palauttaa tiedostonimen sekä osan id:stä, joka varmistaa uniikin nimikkeen. Alla on esimerkki koodista ja sen tulostuksesta:

```
tiedosto, _ := ioutil.TempFile("", "testi")
defer os.Remove(tiedosto.Name())

fmt.Println("Tiedostonimi:", tiedosto.Name())
```

```
Tiedostonimi: /tmp/testi183588789
```

## Syvällinen sukellus:
Väliaikaisten tiedostojen luominen on ollut tärkeä osa ohjelmointia jo pitkään, ja niitä käytetään usein esimerkiksi ohjelmointikielessä tai tietokannoissa. Go-ohjelmointikielen lisäksi myös muut ohjelmointikielet tarjoavat vastaavia toimintoja, kuten Java ```File.createTempFile()```. Väliaikaisten tiedostojen luominen on myös tärkeää tietoturvan kannalta, sillä ne auttavat estämään ei-toivotun tiedoston käsittelyn luvattomilta käyttäjiltä.

## Katso myös:
- Go:n viralliset dokumentaatiot väliaikaisten tiedostojen luomisesta: https://golang.org/pkg/io/ioutil/#TempFile
- Java:n vastaava funktio väliaikaisten tiedostojen luomiseen: https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-
- Tietoturvavinkkejä väliaikaisten tiedostojen käytöstä: https://www.sans.org/white-papers/2131/