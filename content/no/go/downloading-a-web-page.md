---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å laste ned en nettside betyr å hente data fra en server til din lokale maskin. Programmerere gjør dette for å analysere websidens innhold, lagre den for senere bruk eller skrape data.

## Hvordan:
Laste ned en nettside i Go er enkelt. Her er et grunnleggende eksempel:

```Go
package main

import (
	"io/ioutil"
	"net/http"
)

func main() {
	resp, err := http.Get("http://example.com/")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		panic(err)
	}
	println(string(body))
}
```
Koden over vil laste ned nettsiden "http://example.com/" og skrive innholdet til konsollen.

## Dyp Dykk
Historisk sett har folk lastet ned nettsider fra før internetts fødsel - tenk på teletype maskiner. 

Noen andre måter å laste ned nettsider inkluderer bruk av `curl` (for kommandolinjer), nettleser-extensjoner, eller Python's `requests` bibliotek for å nevne noen.

Når det gjelder Go, vil `http.Get()` sende en HTTP GET forespørsel, og `ioutil.ReadAll()` leser svaret. 

Hvis du trenger å håndtere cookies, autentisering eller andre komplekse oppgaver, bør du bruke HTTP klienten i `http.Client`.

## Se Også
For mer detaljerte informasjon eller relaterte emner, sjekk ut disse linkene:

1. Go sin offisielle net/http pakke dokumentasjon: https://golang.org/pkg/net/http/
2. Et mer detaljert blikk på HTTP i Go: https://golang.org/src/net/http/
3. Web scraping med Go: https://edmundmartin.com/writing-a-web-crawler-in-golang/.