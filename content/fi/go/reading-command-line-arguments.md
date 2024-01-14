---
title:    "Go: Komentoriviparametrien lukeminen"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Miksi lukea komentoriviparametreja Go-ohjelmoinnissa?

Tämä blogikirjoitus on tarkoitettu kaikille ohjelmoijille, jotka haluavat oppia lukemaan komentoriviparametreja Go-ohjelmoinnissa. Komentoriviparametrit ovat tärkeä osa monia komentorivisovelluksia ja niiden ymmärtäminen on tärkeää monissa ohjelmointiprojekteissa.

## Miten lukea komentoriviparametreja

Go-ohjelmoinnin komentoriviparametrien lukeminen on yksinkertaista ja helppoa. Alla olevassa koodiesimerkissä näytämme, kuinka voit lukea parametreja käyttäen ´args´-muuttujaa ja ´len´-funktiota.

```Go
package main

import (
  "fmt"
  "os"
)

func main() {
  args := os.Args
  fmt.Println("Komentoriviparametrit: ", args[1:])
}
```

Kun ajat edellä olevan esimerkin ohjelmaa komentoriviltä käsin antaen sille parametreja, ohjelma tulostaa ne kaikki. Esimerkiksi, jos ajat `go run arguments.go hello world`, ohjelma tulostaa "Komentoriviparametrit: hello world".

## Tarkempi tarkastelu komentoriviparametreista

Komentoriviparametrien lukeminen Go-ohjelmoinnissa on vain yksi osa asiaa. Syvempään ymmärrykseen pääsee tutkimalla myös muita komentorivin ominaisuuksia, kuten ympäristömuuttujia ja käyttäjän syötteitä. Suosittelemme kokeilemaan erilaisia skenaarioita ja tutkimaan niiden toimintaa.

# Katso myös

- [Luettelo Go-kielessä käytettävistä komentoriviparametreista](https://gobyexample.com/command-line-arguments)
- [Go-kielen opas komentoriviparametrien käsittelyyn](https://golang.org/pkg/flag/)

Kirjoittanut: Sinun nimesi