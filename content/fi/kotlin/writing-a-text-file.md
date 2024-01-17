---
title:                "Tiedoston kirjoittaminen"
html_title:           "Kotlin: Tiedoston kirjoittaminen"
simple_title:         "Tiedoston kirjoittaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Mikä ja miksi?

Tekstitiedoston kirjoittaminen tarkoittaa yksinkertaisesti tiedon tallentamista tekstimuodossa. Ohjelmoijat tekevät tätä esimerkiksi tallentaakseen käyttäjän syöttämän datan tai tulostamaan tietoja sovelluksesta.

# Miten:

```Kotlin 
val tiedosto = File("file.txt")
tiedosto.writeText("Tämä on esimerkki tekstitiedostosta")
```

Tässä esimerkissä luomme tiedoston nimeltä "file.txt" ja kirjoitamme siihen tekstin. Tämän jälkeen voimme lukea tiedoston sisällön käyttäen File-luokan readText() -metodia.

```
Tämä on esimerkki tekstitiedostosta
```

# Syvemmälle:

Tekstitiedostojen kirjoittamista on käytetty jo vuosikymmenten ajan tietokoneohjelmoinnissa. Nykyään on olemassa myös muita tapoja tallentaa ja käsitellä dataa, kuten tietokannat tai JSON-tiedostot.

Tekstitiedostojen kirjoittaminen toteutetaan yleensä käyttäen tietokoneen tiedostojärjestelmään liittyviä komentoja, joten eri käyttöjärjestelmät saattavat vaatia erilaisia koodinpätkiä.

# Katso myös:

[Java IO-tiedostonkirjoitus](https://docs.oracle.com/javase/tutorial/essential/io/file.html)