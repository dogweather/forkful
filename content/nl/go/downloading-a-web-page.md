---
title:                "Een webpagina downloaden"
date:                  2024-01-28T21:59:01.704249-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een webpagina downloaden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een webpagina downloaden betekent het ophalen van de inhoud ervan via HTTP. Programmeurs doen dit om te communiceren met webservers, gegevens te schrapen of de uptime van de site te monitoren.

## Hoe te:

In Go is het downloaden van een webpagina een fluitje van een cent met het `net/http` pakket. Hier is het kort samengevat:

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	resp, err := http.Get("http://example.com")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		panic(err)
	}

	fmt.Println(string(body))
}
```

Voer het uit, en je krijgt de HTML van `http://example.com` op je scherm, plus of min enkele HTTP-headers.

## Diepgaande Duik

Vroeger was webcontent ophalen een wilde westen van socketprogrammering en handgemaakte HTTP-verzoeken. Nu nemen bibliotheken zoals Go's `http` het zware werk uit onze handen.

Waarom niet gewoon `curl` of `wget`? Automatisering, mijn vriend. Het inbedden van de downloadlogica in je code maakt het herhaalbaar en integreerbaar.

Onder de motorkap maakt `http.Get` een GET-verzoek, beheert cookies, en meer. Je kunt timeouts, headers controleren, en zo diep gaan als aangepaste transports. Maar dat is een verhaal voor een andere dag.

Wat betreft alternatieven, je zou `http.Client` kunnen overwegen als je meer controle nodig hebt, of pakketten van derden zoals `gorequest` voor een andere smaak.

## Zie Ook

- De Go net/http pakketdocumentatie: https://pkg.go.dev/net/http
- Effectief Go voor beste praktijken: https://golang.org/doc/effective_go
- Go by Example voor meer praktische voorbeelden: https://gobyexample.com/
