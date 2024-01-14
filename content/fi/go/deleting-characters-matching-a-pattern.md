---
title:    "Go: Mallille vastaavien merkkien poistaminen"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointiprojekteissa saattaa olla tarve poistaa tietynlaiset merkit tai merkkijonot tekstitiedostoista. Tämä voi johtua esimerkiksi tiedoston formatoinnista tai tietyn algoritmin vaatimuksista. Go-ohjelmointikielen avulla tämän tehtävän suorittaminen on helppoa ja tehokasta.

## Miten

Voit poistaa merkkejä, jotka vastaavat tiettyä kaavaa, käyttämällä Go-kielen `strings` ja `regexp` paketteja. Esimerkiksi, seuraava koodin avulla voit poistaa kaikki numerot `text` merkkijonosta:

```Go
package main

import (
  "fmt"
  "regexp"
  "strings"
)

func main() {
  text := "Tervetuloa2020GoMaailmaan"
  
  r := regexp.MustCompile(`\d+`)
  newText := r.ReplaceAllString(text, "")

  fmt.Println(newText) // Tulostaa: "TervetuloaGoMaailmaan"
}
```

## Syväsyvennys

Yllä oleva esimerkki käyttää regulaaarilausekkeita poistamaan numerot merkkijonosta. `strings` ja `regexp` pakettien lisäksi Go-kieli tarjoaa myös muita vaihtoehtoja merkkien poistamiseen, kuten `bytes` paketin `Replace` -metodi ja `strings` paketin `ReplaceAll` ja `Trim` -metodit.

Syvässä sukelluksessa voit myös oppia lisää regulaaarilausekkeista ja niiden käytöstä merkkijonojen muokkaamisessa. Näiden tietojen avulla voit luoda monimutkaisempia tapoja poistaa merkkejä, jotka vastaavat tietyä kaavaa.

## Katso myös

- [Go-kirjasto: strings](https://golang.org/pkg/strings/)
- [Go-kirjasto: regexp](https://golang.org/pkg/regexp/)
- [Merkkijonojen poistaminen Go-kielen avulla](https://www.digitalocean.com/community/tutorials/how-to-manipulate-strings-in-go)