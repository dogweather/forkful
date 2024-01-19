---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad och varför?

Att ladda ner en webbsida innebär att hämta HTML-koden bakom en webbsida till din dator. Programmerare gör detta för att kunna bearbeta webbsidans innehåll, exempelvis för att skrapa data eller testa webbsidor.

## Så här gör du:

I Go kan du hämta och läsa in en webbsida med hjälp av paketen `net/http` och `io/ioutil`.

```Go
package main

import (
	"fmt"
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
	fmt.Println(string(body))
}
```
När du kör koden ovan kommer du att se HTML-innehållet i webbsidan http://example.com/ skrivas ut i konsolen.

## Fördjupning

Hämtning av webbsidor har ägt rum ända sedan webbens begynnelse och metoder för att göra detta har utvecklats och förfinats över tid. Alternativ till Go's inbyggda paket `net/http` och `io/ioutil` inkluderar tredjepartspaket som `colly` och `goquery` som kan underlätta och förbättra prestandan.

Vid implementation är det viktigt att vara medveten om resurshantering. I Go-koden ovan kommer `defer resp.Body.Close()` att se till att nätverksresurserna släpps när funktionen `main` är klar, även om ett fel skulle inträffa. Detta är avgörande för att inte läcka resurser.

## Se mer:

Du kan läsa mer om Go och nedladdningar på dessa länkar:

- Go's officiella dokumentation: https://golang.org/pkg/
- Paketet 'colly': https://github.com/gocolly/colly
- Paketet 'goquery': https://github.com/PuerkitoBio/goquery