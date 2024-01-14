---
title:    "Gleam: Merkkijonojen yhdistäminen"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi meidän pitäisi yhdistää merkkijonoja ohjelmoidessamme? Yksinkertaisesti sanottuna, merkkijonojen yhdistäminen mahdollistaa erilaisten tietojen yhdistämisen yhdeksi kokonaisuudeksi, mikä helpottaa tietojen hallintaa ja käsittelemistä.

## Kuinka tehdä se

Gleam-ohjelmoinnissa merkkijonojen yhdistäminen tapahtuu käyttämällä "++" -operaattoria. Tämä operaattori yhdistää kaksi merkkijonoa yhdeksi ja tuottaa uuden merkkijonon. Alla on esimerkki:

```Gleam
let nimi = "Jussi"
let tervehdys = "Hei, minun nimeni on " ++ nimi ++ "!"

IO.println(tervehdys)
```
Tulostaa: Hei, minun nimeni on Jussi!

## Syvemmälle Katsoen

Yhdistämisen lisäksi Gleam tarjoaa myös muita mahdollisuuksia merkkijonojen hallintaan. Voit esimerkiksi käyttää Gleam-standardikirjaston moduulia "string" löytääksesi merkkijonosta tiettyjä merkkejä tai merkkijonon osia. Voit myös käyttää "string.split" -funktiota jakamaan merkkijonon tietyn merkin tai merkkijonon mukaan. Näitä ja muita toimintoja voi tutkia lisää Gleam-dokumentaatiossa.

## Katso myös

- [Gleam-dokumentaatio](https://gleam.run/book/stdlib.html#string)
- [Merkkijonojen käsittelyn perusteet C-kielen avulla](https://www.tutorialspoint.com/cprogramming/string_handling_in_c.htm)
- [10 työkalua, joilla parannat merkkijonojen hallintaa JavaScriptissä](https://www.freecodecamp.org/news/10-tools-to-power-up-your-string-manipulations-in-javascript-78a0f9157720/)