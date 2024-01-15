---
title:                "Verkkosivun lataaminen"
html_title:           "Go: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Web-sivujen lataaminen on tärkeä osa verkkokehitystä ja se on usein tarpeen, kun haluamme hakea tietoja sivuilta tai luoda omia sovelluksia, jotka käyttävät verkkosisältöä.

## Miten

Lataaessa verkkosivua Go-kielellä, voit käyttää `http.Get()` -funktiota, joka palauttaa vastauksen ja virheen. Esimerkiksi:

```Go
res, err := http.Get("https://www.esimerkkisivusto.com")
```
Tämän jälkeen voit käyttää `res` -muuttujaa saadaksesi tiedon latauksen tilasta ja sisällöstä. Esimerkiksi voit tulostaa vastauksena saadun tekstin konsolille:

```Go
body, err := ioutil.ReadAll(res.Body)
fmt.Println(string(body))
```
Tässä olemme käyttäneet myös `ioutil` -pakettia tiedon lukemiseksi `res.Body` -muuttujasta.

## Syväsukellus

Web-sivujen lataamisen taustalla on protokolla nimeltä HTTP (Hypertext Transfer Protocol). HTTP-metodit, kuten `GET` käytetään pyytämään tietoa palvelimelta. Kun käytämme `http.Get()` -funktiota, luomme pyynnön annetulle URL-osoitteelle ja odotamme vastausta palvelimelta. Vastausta käsitellään sitten vastaavasti, kuten esiteltiin edellisessä osiossa.

## Katso myös

- [Go-nettipalvelujen opas](https://golang.org/pkg/net/http/)
- [HTTP-protokollan selitys](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)