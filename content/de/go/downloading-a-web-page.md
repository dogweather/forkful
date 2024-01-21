---
title:                "Webseite herunterladen"
date:                  2024-01-20T17:44:10.762153-07:00
model:                 gpt-4-1106-preview
simple_title:         "Webseite herunterladen"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Das Herunterladen einer Webseite bedeutet, ihre Daten abzurufen und lokal zu speichern. Programmierer tun dies, um Inhalte zu analysieren, zu archivieren oder Offline-Zugang zu ermöglichen.

## How to: (Wie geht das:)
```Go
package main

import (
	"io"
	"net/http"
	"os"
)

func main() {
	// Request the HTML page.
	resp, err := http.Get("http://example.com")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	// Create output file.
	outFile, err := os.Create("example.html")
	if err != nil {
		panic(err)
	}
	defer outFile.Close()

	// Copy data from the response to the output file.
	_, err = io.Copy(outFile, resp.Body)
	if err != nil {
		panic(err)
	}

	println("Webseite heruntergeladen.")
}
```
Beispielausgabe:

```
Webseite heruntergeladen.
```

## Deep Dive (Tiefer eintauchen)
Früher wurde zum Herunterladen von Webseiten oft `curl` oder `wget` in der Kommandozeile genutzt. In Go greifen wir auf das `http` Paket zurück, das Teil der Standardbibliothek ist und HTTP-Anfragen vereinfacht. Es gibt auch Drittanbieter-Pakete wie `colly`, die erweiterte Funktionen bieten, etwa für Web Scrapping.

Bei der Implementierung ist es wichtig, die `io` und `os` Pakete zum Schreiben der Daten in eine Datei zu nutzen. Achte darauf, `resp.Body.Close()` und `outFile.Close()` in `defer` Statements zu setzen, um Ressourcen freizugeben.

## See Also (Siehe auch)
- Go `http` package documentation: [https://pkg.go.dev/net/http](https://pkg.go.dev/net/http)
- Go by Example - HTTP Clients: [https://gobyexample.com/http-clients](https://gobyexample.com/http-clients)
- `colly` - ein Web Scraping Framework für Go: [http://go-colly.org](http://go-colly.org)