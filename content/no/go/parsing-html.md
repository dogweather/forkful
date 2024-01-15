---
title:                "Parsing html"
html_title:           "Go: Parsing html"
simple_title:         "Parsing html"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor vil noen ønske å analysere/parse HTML? Vel, det er flere grunner til dette. En av de viktigste årsakene er at det brukes i utviklingen av nettsider og applikasjoner for å hente ut viktig informasjon som ligger innenfor HTML-koden. Ved å analysere og manipulere HTML kan man automatisere visse oppgaver og gjøre det enklere å hente ut spesifikke data. Derfor er det viktig å ha et godt forståelse av hvordan man kan parse HTML ved hjelp av Go-programmeringsspråket.

## Hvordan
Go har mange gode verktøy for å gjøre parsing av HTML enklere. La oss se på et enkelt eksempel for å vise hvordan vi kan starte:

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	// Gå til en nettside og hent HTML-kode
	resp, err := http.Get("https://www.example.com")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	// Les HTML-koden og skriv ut den
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		panic(err)
	}
	fmt.Println(string(body))
}
```

La oss nå se på et eksempel der vi ønsker å hente ut all tekst innenfor et bestemt HTML-element. I dette eksempelet bruker vi pakken "golang.org/x/net/html" som gir oss mulighet til å analysere HTML-koden mer nøyaktig:

```Go
package main

import (
	"fmt"
	"log"
	"net/http"

	"golang.org/x/net/html"
)

func main() {
	resp, err := http.Get("https://www.example.com")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	// Bruk HTML-parser for å navigere gjennom HTML-koden
	doc, err := html.Parse(resp.Body)
	if err != nil {
		log.Fatal(err)
	}

	// Finn alle <p> elementene og hent ut deres tekstinnhold
	var getText func(*html.Node)
	getText = func(n *html.Node) {
		if n.Type == html.ElementNode && n.Data == "p" {
			fmt.Println(n.FirstChild.Data)
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			getText(c)
		}
	}
	getText(doc)
}
```

Dette er bare to enkle eksempler på hvordan man kan parse HTML-kode ved hjelp av Go. Det finnes mange andre funksjoner og pakker som kan hjelpe deg med å hente ut spesifikke data eller manipulere HTML-koden på forskjellige måter. Det er bare å eksperimentere og utforske de forskjellige mulighetene.

## Deep Dive
Nå som vi har sett på noen eksempler, la oss dykke litt dypere inn i hvordan parser HTML egentlig fungerer. Go har et enkelt og effektivt verktøy kalt "the tokenizer", som er ansvarlig for å lese gjennom HTML-koden og gjøre den tilgjengelig for deg å behandle. Når HTML-koden blir "tokenisert", blir den delt opp i mindre biter som kan leses og manipuleres enkeltvis.

I tillegg har Go også pakken "golang.org/x/net/html/atom", som gir en liste over alle HTML-elementene som kan forekomme. Dette kan være nyttig når man ønsker å hente ut spesifikke data basert på elementets navn.

## Se også
Sjekk ut disse ressursene for å lære mer om å parse HTML i Go:

- Offisiell dokumentasjon for nett-pakken: https://golang.org/pkg/net/http/
- Gode eksempler på å parse HTML med Go: https://github.com/PuerkitoBio/goquery
- En detaljert guide for å manipulere og hente ut data fra HTML-kode: https://tutorialedge.net/golang/parsing-html-with-go/