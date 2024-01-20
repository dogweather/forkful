---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/parsing-html.md"
---

{{< edit_this_page >}}

#Golangilla HTML:n jäsentäminen: Miksi ja miten?

## Mikä & Miksi?

HTML-jäsentäminen on prosessi, jossa muunnetaan HTML-koodi rakenteelliseksi representaatioksi. Ohjelmoijat tekevät tämän, jotta voivat helposti hakea, lisätä, muokata tai poistaa HTML-elementtejä ohjelmallisesti.

## Miten:
 
**Go:ssa** HTML:n jäsentäminen on yksinkertaista. Esimerkkinä jäsennämme Go:n standardikirjaston **net / html** -paketin avulla alla olevan HTML-koodin.

```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"strings"
)

func main() {
	s := `<p>Hei maailma!</p>`
	doc, _ := html.Parse(strings.NewReader(s))
	var f func(*html.Node)
	f = func(n *html.Node) {
		if n.Type == html.ElementNode && n.Data == "p" {
			fmt.Println(n.FirstChild.Data)
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			f(c)
		}
	}
	f(doc)
}
```
Suoritettaessa tämä koodi, tulostuu seuraava:

`Hei maailma!`

Tässä me luomme HTML-dokumentin, jolla on vain yksi alue-elementti ja tulostetaimme sen sisällön.

## Syvällisemmin

HTML-jäsentämisen historia juontaa juurensa WWW:n alkuaikoihin. Aluksi HTML-dokumentit luotiin ja muokattiin manuaalisesti, mutta kun web kasvoi, tämä kävi yhä vaikeammaksi. Tämä johti ohjelmallisen HTML-jäsentämisen kehitykseen.

HTML-jäsentämisen vaihtoehtoina on monia muita menetelmiä, kuten regexien käyttö tai jopa manuaalinen merkkijonojen käsittely. Kuitenkin, nämä ovat yleensä virhealttiimpia ja vaikeammin kuin olisi suotavaa. Siksi strukturoitu jäsentäminen, kuten Go:ssa `html / parse` paketin avulla, on suositeltavin tapa.

Go:n `html / parse` -paketti toteuttaa jäsentämisen luomalla puurakenteita, joista kukin solmu vastaa HTML-dokumentin eri osaa. Liikutaan puun läpi käyttämällä syvyyssuuntaista ensinnäkijän hakua.

## Katso myös

- Go:n HTML Parse paketti: https://godoc.org/golang.org/x/net/html
- w3c:n HTML Parse algoritmi: https://www.w3.org/TR/html50/parsing.html
- Matala XML-vastaava: https://golang.org/pkg/encoding/xml/