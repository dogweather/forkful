---
title:                "Einen HTTP-Request senden"
date:                  2024-01-20T17:59:35.688523-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen HTTP-Request senden"

category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Requests sind der Kern des Web: Sie holen Daten von Servern. Programmierer nutzen sie, um APIs anzusprechen, Webseiten-Inhalte zu scrappen oder Services zu integrieren.

## So geht's:
Ein einfacher GET-Request in Go sieht so aus:

```go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	resp, err := http.Get("https://api.example.com/data")
	if err != nil {
		fmt.Println("Request fehlgeschlagen:", err)
		return
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		fmt.Println("Lesen des Bodys fehlgeschlagen:", err)
		return
	}

	fmt.Println(string(body))
}
```
Erfolgreiche Ausgabe könnte sein:
```
{"name":"Max Mustermann","age":42}
```

## Tiefgang
Ursprunglich waren HTTP-Requests Ansätze zur Kommunikation zwischen Browsern und Servern. Heute benutzen sie auch unterschiedliche Programme zur Interaktion. Andere Methoden, wie die Low-Level Sockets, bieten mehr Kontrolle sind aber komplizierter. In Go kümmern sich das `net/http`-Paket und ergänzende Bibliotheken um die Details der HTTP-Kommunikation, wie das Handhaben von Cookies, das Setzen von Headern oder SSL.

## Siehe auch
- Go Dokumentation für das `net/http`-Paket: https://golang.org/pkg/net/http/
- "The Go Programming Language" von Alan A. A. Donovan & Brian W. Kernighan, speziell Kapitel 7 für Webanwendungen.
- Go by Example mit HTTP-Request-Beispielen: https://gobyexample.com/http-clients

Bitte beachtet, dass die angegebenen URLs als Beispiele dienen. Stellt sicher, dass Ihr auf echte Ressourcen mit den entsprechenden Berechtigungen zugreift.
