---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:05:31.506873-07:00
description: "Kuinka: HTML:n j\xE4sent\xE4miseen Go:ssa k\xE4ytet\xE4\xE4n yleens\xE4\
  \ `goquery`-pakettia tai standardikirjaston `net/html`-pakettia. T\xE4ss\xE4 on\
  \ perusesimerkki k\xE4ytt\xE4en\u2026"
lastmod: '2024-03-13T22:44:56.047919-06:00'
model: gpt-4-0125-preview
summary: "HTML:n j\xE4sent\xE4miseen Go:ssa k\xE4ytet\xE4\xE4n yleens\xE4 `goquery`-pakettia\
  \ tai standardikirjaston `net/html`-pakettia."
title: "HTML:n j\xE4sent\xE4minen"
weight: 43
---

## Kuinka:
HTML:n jäsentämiseen Go:ssa käytetään yleensä `goquery`-pakettia tai standardikirjaston `net/html`-pakettia. Tässä on perusesimerkki käyttäen `net/html`-pakettia kaikkien linkkien poimimiseen verkkosivulta:

```go
package main

import (
    "fmt"
    "golang.org/x/net/html"
    "net/http"
)

func main() {
    // Hae HTML-dokumentti
    res, err := http.Get("http://esimerkki.com")
    if err != nil {
        panic(err)
    }
    defer res.Body.Close()

    // Jäsennä HTML-dokumentti
    doc, err := html.Parse(res.Body)
    if err != nil {
        panic(err)
    }

    // Funktio DOMin rekursiiviseen läpikäymiseen
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

    // Käy läpi DOM
    f(doc)
}
```

Esimerkkituloste (olettaen, että `http://esimerkki.com` sisältää kaksi linkkiä):

```
http://www.iana.org/domains/esimerkki
http://www.iana.org/domains/varattu
```

Tämä koodi pyytää HTML-sivua, jäsentää sen ja käy rekursiivisesti läpi DOM-rakenteen löytääkseen ja tulostaakseen kaikkien `<a>`-tagien `href`-attribuutit.

## Syväsukellus
`net/html`-paketti tarjoaa perustyökalut HTML:n jäsentämiseen Go:ssa, toteuttaen suoraan HTML5-standardin määrittelemät tokenisointi- ja puurakennusalgoritmit. Tämä matalan tason lähestymistapa on tehokas, mutta voi olla monimutkainen monimutkaisemmissa tehtävissä.

Senaikaisesti kolmannen osapuolen `goquery`-paketti, jonka on inspiroinut jQuery, tarjoaa korkeamman tason rajapinnan, joka yksinkertaistaa DOM-manipulointia ja -traversointia. Se mahdollistaa kehittäjien kirjoittaa tiiviimpää ja ilmaisuvoimaisempaa koodia tehtäviin, kuten elementin valintaan, attribuuttien poimintaan ja sisällön manipulointiin.

Kuitenkin, `goquery`n mukavuus tulee lisäriippuvuuden kustannuksella ja mahdollisesti hitaamman suorituskyvyn vuoksi sen abstraktiotason takia. Valinta `net/html` ja `goquery` (tai muiden jäsentämiskirjastojen) välillä riippuu projektin erityisvaatimuksista, kuten suorituskyvyn optimoinnin tarpeesta tai käytön helppoudesta.

Historiallisesti Go:n HTML-jäsentäminen on kehittynyt perus merkkijono-operaatioista edistyneisiin DOM-puurakenteen manipulointitekniikoihin, heijastaen kielen kasvavaa ekosysteemiä ja yhteisön kysyntää vahvoille verkkosivujen kaapimisen ja datan poimintatyökaluille. Huolimatta natiiveista kyvyistä, kolmansien osapuolien kirjastojen, kuten `goquery`, suosio korostaa Go-yhteisön mieltymystä modulaariseen, uudelleen käytettävään koodiin. Kuitenkin suorituskykyä vaativissa sovelluksissa ohjelmoijat saattavat edelleen suosia `net/html`-pakettia tai jopa turvautua regexiin yksinkertaisissa jäsentämistehtävissä, pitäen mielessä regex-pohjaisen HTML-jäsentämisen synnynnäiset riskit ja rajoitukset.
