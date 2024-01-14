---
title:    "Gleam: Väliaikaistiedoston luominen"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksikö?

Jokaisella ohjelmointikielellä on tapansa käsitellä muuttuvia tiedostoja ja tietoa. Gleamilla, funktionaalisella ohjelmointikielellä, on oma tapansa luoda väliaikaisia tiedostoja. Tässä artikkelissa sukellamme Gleamin `File`-moduuliin ja opimme, miksi ja miten luoda väliaikaisia tiedostoja tässä ohjelmointikielessä.

## Kuinka luoda väliaikainen tiedosto Gleamilla

Gleamin `File`-moduulista löytyy toiminto `Temporary.file()`, joka luo uuden väliaikaisen tiedoston annetulla tiedostopäätteellä ja palauttaa tiedoston nimen string-muodossa. Toiminnon ensimmäisen parametrin määritellään oletusarvoisesti olevan tiedostopääte `.tmp`. Alla on esimerkki siitä, kuinka voit luoda väliaikaisen tiedoston Gleam-ohjelmassa:

```Gleam
import File

fn main() {
  let tmp_file = File.Temporary.file()
  // tmp_file = "/var/folders/vv/dfs78ndf22skfnh/tmp123456.tmp"
  File.write(tmp_file, "Tämä on väliaikainen tiedosto")
  let text = File.read(tmp_file)
  // text = "Tämä on väliaikainen tiedosto"
}
```

## Syvemmälle väliaikaisten tiedostojen luomiseen

`File.Temporary.file()` käyttää Gleamin `File`-moduulin `open()` ja `close()` -toimintoja luodakseen väliaikaisen tiedoston. `Temporary.file()`-toiminto myös luo uniikin tiedostonimen käyttäen timestamppia, satunnaislukua ja annettua tiedostopäätettä. Tiedosto automaattisesti poistetaan, kun se suljetaa `File.close()` -toiminnolla tai ohjelma päättyy.

## Katso myös

- `File`-moduulidokumentaatio: https://gleam.run/modules/file/latest/
- Gleamin virallinen verkkosivusto: https://gleam.run/
- Lisää Gleam-ohjelmointikielestä: https://en.wikipedia.org/wiki/Gleam_(programming_language)