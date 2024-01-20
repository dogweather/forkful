---
title:                "HTML:n jäsentäminen"
date:                  2024-01-20T15:31:56.573125-07:00
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
HTML:n jäsentäminen on prosessi, jossa HTML-dokumentin sisältö muutetaan rakenteiseksi dataksi, jonka ohjelmat voivat ymmärtää ja käsitellä. Koodarit jäsentävät HTML:ää, jotta voivat suorittaa data-analyysiä, web-sisällön kaappauksia tai testata web-sovelluksia.

## How to: (Kuinka tehdään:)
```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"net/http"
	"strings"
)

func main() {
	resp, err := http.Get("http://example.com")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	root, err := html.Parse(resp.Body)
	if err != nil {
		panic(err)
	}

	var f func(*html.Node)
	f = func(n *html.Node) {
		if n.Type == html.ElementNode && n.Data == "a" {
			for _, a := range n.Attr {
				if a.Key == "href" {
					fmt.Println(a.Val)
					break
				}
			}
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			f(c)
		}
	}
	f(root)
}
```

Sample output:
```
http://www.iana.org/domains/example
```

## Deep Dive: (Syväsukellus)
HTML:n jäsentämistä on käytetty Webin alkuajoista lähtien kun ohjelmoijat halusivat ymmärtää ja hyödyntää verkkosivujen rakennetta. Golangissa x/net/html -kirjasto on nykyinen työkalu HTML:n jäsentämiseen. Se tarjoaa DOM-puuta muistuttavan rakenteen, mutta ei täyttä DOM APIa.

Vaihtoehtoja ovat muun muassa BeautifulSoup Pythonissa ja Nokogiri Rubyssa, jotka on suunniteltu helpottamaan HTML:n ja XML:n käsittelyä. Golangissa käytetään puuta käsitteleviä funktioita selkeyden vuoksi ja koska kieli suosii kompositiota perinnön sijasta.

Jäsentämisen toteutuksessa huomioon otetaan HTML:n löyhät standardit: dokumentit voivat olla epätäydellisiä tai virheellisiä, mutta x/net/html pyrkii olemaan joustava ja käsittelemään näitä tilanteita armoillisesti.

## See Also: (Katso Myös)
- Go’s html package documentation: [https://pkg.go.dev/golang.org/x/net/html](https://pkg.go.dev/golang.org/x/net/html)
- Go by Example: HTTP Clients: [https://gobyexample.com/http-clients](https://gobyexample.com/http-clients)