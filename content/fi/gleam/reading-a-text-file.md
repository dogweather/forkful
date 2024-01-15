---
title:                "Tiedoston lukeminen"
html_title:           "Gleam: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Lukeminen on olennainen osa ohjelmointia ja tekstifilun lukeminen on usein välttämätöntä monissa ohjelmointitehtävissä. Tässä artikkelissa opimme, kuinka voit lukea tekstifiluja Gleam-ohjelmointikielellä ja miksi se voi olla hyödyllistä.

## Kuinka

Text-filun lukeminen Gleamilla on helppoa. Se tapahtuu `File.read/1` funktion avulla, joka ottaa yhden argumentin, joka on tekstifilun polku.

```Gleam
let result = File.read("tiedostonimi.txt")
```

Tämä palauttaa tekstifilun sisällön merkkijonona. Voit sitten käsitellä tätä merkkijonoa haluamallasi tavalla. Esimerkiksi tulostaaksesi tekstifilun sisällön voit käyttää `io` moduulia.

```Gleam
import gleam/io

let file_content = File.read("tiedostonimi.txt")

io.println(file_content)
```

## Syventävä sukellus

Gleam pyrkii helpottamaan tekstifilujen lukemista tarjoamalla selkeitä ja intuitiivisia ohjelmointirakenteita. Voit esimerkiksi halutessasi lukea vain tietyn määrän merkkejä tekstifilusta, voit käyttää `File.read_chunk/2` funktiota. Tämä ottaa kaksi argumenttia, ensimmäinen on tekstifilun polku ja toinen on haluttu lukumäärä merkkejä.

```Gleam
let result = File.read_chunk("tiedostonimi.txt", 50)
```

Tämä palauttaa merkkijonon, joka sisältää 50 ensimmäistä merkkiä tekstifilusta.

## Katso myös

- Gleamin virallinen dokumentaatio: https://gleam.run/
- Ohjeet tekstifilujen lukemiseen Gleamilla: https://hexdocs.pm/gleam/0.14.0/File.html#read/1
- Esimerkkejä tekstifilujen lukemisesta Gleamilla: https://github.com/gleam-lang/gleam/blob/master/examples/io/file_read.gleam