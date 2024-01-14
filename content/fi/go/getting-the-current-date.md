---
title:                "Go: Nykyisen päivämäärän saaminen"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Kun kehität sovelluksia Go-ohjelmointikielellä, voi olla tarpeellista saada ajantasainen päivämäärä tietoon. Tässä blogikirjoituksessa käymme läpi, miten voit helposti hankkia nykyisen päivämäärän Go-kielellä.

## Miten

Päivämäärän hankkiminen Go-kielellä on yksinkertaista käyttäen time-pakettia. Voit aloittaa tuomalla tämän paketin käytettäväksi seuraavasti:

```Go 
import "time" 
```

Tämän jälkeen voit luoda uuden muuttujan, joka sisältää nykyisen ajan tietoon käyttämällä time.Now()-funktiota. Listataan esimerkiksi päivämäärä ja kellonaika seuraavassa koodikatkelmassa:

```Go 
now := time.Now() 
fmt.Println(now.Format("2.1.2006 15:04:05")) 
```

Tämä tulostaisi esimerkiksi seuraavanlaisen tuloksen:

```
9.4.2021 14:12:36
```

Voit myös halutessasi muotoilla päivämäärän ja kellonajan haluamallasi tavalla käyttämällä Format()-funktiota. Voit tarkastella kaikkia käytettävissä olevia muotoiluvaihtoehtoja Go:n dokumentaatiosta.

## Syvällisempi tarkastelu

Time-paketti sisältää myös muita hyödyllisiä toimintoja, kuten ajastimen ja tarkempia aikavälin laskentaominaisuuksia. Voit tutustua näihin syvemmin Go:n dokumentaatiosta.

## Katso myös

- Go:n time-paketin dokumentaatio: https://golang.org/pkg/time/
- Go-ohjelmoinnin perusteet: https://www.golang-book.com/books/intro
- Go:n virallinen nettisivu: https://golang.org/