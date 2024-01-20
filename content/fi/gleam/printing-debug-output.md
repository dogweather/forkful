---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Tehostaaksemme koodin tarkistamista ja virheiden paikallistamista, tulostamme erityistä tietoa, jonka nimitämme debug-tulosteeksi. Se on kuin majakka koodissamme, joka ohjaa meitä ohjelmoimaan paremmin ja tehokkaammin. 

## Kuinka:
Gleamissa debug-tulostus toteutetaan funktiolla `io.debug/1` ja sen käyttö on seuraavanlainen:

```Gleam
import gleam/io

fn main() {
  let list = list.from([1, 2, 3])

  // Tulostetaan lista debug-tietoina
  io.debug(list)
}
```
Tulostus näyttää seuraavalta:
```
Debug: [1, 2, 3]
```
## Deep Dive
Aiemmin Gleam-ohjelmassa debug-tietojen tulostaminen oli hieman hankalaa. Historiallisesti Gleam käytti Erlangin 'io:format' -funktiota, joka oli kömpelö ja sekava Gleam-kehittäjille. 

Vaihtoehtoiset debug-tulostusmenetelmät löytyvät useista muista ohjelmointikielistä, kuten JavaScriptin `console.log` tai Pythonin `print`. Gleamin `io.debug/1` on nyt yksinkertaisempi ja helpompi tapa.

Gleamin `io.debug/1` palauttaa aiemman arvon käytännön syistä, ja nyt käytettävissä on myös `io.debug/2`, joka ottaa merkkijonon lisättäväksi tulostettuun debug-tietoon. 

## Katso Myös
- [Gleam Io-Moduuli](https://hexdocs.pm/gleam_stdlib/gleam/io.html#debug/1)
- [Gleam-kielen opas](https://gleam.run/book/)
- [Io-debug-funktio lähdekoodissa](https://github.com/gleam-lang/stdlib/blob/main/src/gleam/io.gleam)