---
title:                "Nedlasting av en nettside"
date:                  2024-01-20T17:44:03.492972-07:00
model:                 gpt-4-1106-preview
simple_title:         "Nedlasting av en nettside"

category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? - Hva & Hvorfor?
Å laste ned en nettside betyr å hente HTML-innholdet fra en URL. Programmerere gjør dette for å skrape data, teste apps eller analysere innhold.

## How to: - Hvordan:
```go
package main

import (
	"fmt"
	"io"
	"net/http"
	"os"
)

func DownloadWebPage(url string) error {
	// Få en HTTP GET-forespørsel til URL-en
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	// Opprett en fil for å lagre nettsiden
	file, err := os.Create("website.html")
	if err != nil {
		return err
	}
	defer file.Close()

	// Kopier HTML-innholdet fra HTTP-responsen til filen
	_, err = io.Copy(file, resp.Body)
	return err
}

func main() {
	url := "http://example.com"
	err := DownloadWebPage(url)
	if err != nil {
		fmt.Println("Feil ved nedlasting av nettsiden:", err)
		return
	}
	fmt.Println("Nettsiden ble lastet ned!")
}
```

## Deep Dive - Dypdykk:
Før Go, brukte utviklere ofte verktøy som `wget` eller `curl` for å hente nettsider, men disse krevde eksterne programmatiske tillatelser. Go tilbyr en standardpakke `net/http` som gjør HTTP-forespørsler enklere å implementere i kode. Alternativt kan du bruke tredjepartsbiblioteker som `Colly` for mer avansert web-skraping. Når det gjelder implementeringsdetaljer, er det viktig å lukke ressurser som HTTP-respons og filer for å unngå ressurslekkasjer.

## See Also - Se Også:
- Go's net/http package documentation: [https://pkg.go.dev/net/http](https://pkg.go.dev/net/http)
- An introduction to web scraping with Go and Colly: [https://medium.com/](https://medium.com/@thedevsaddam/web-scraping-in-go-ebc7f4f327d8)
- Understanding the basics of HTTP: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)
