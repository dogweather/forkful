---
title:                "Att skicka en http-begäran"
html_title:           "Go: Att skicka en http-begäran"
simple_title:         "Att skicka en http-begäran"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran går ut på att skicka en begäran till en webbplats eller webbserver för att generera ett svar. Programmerare gör detta för att samla data, interagera med externa tjänster eller ta hand om webb-API:er.

## Hur Man Gör:
Här är ett basic exempel på hur man skickar en HTTP GET-begäran i Go:

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	response, err := http.Get("http://webcode.me")
	if err != nil {
		fmt.Println(err)
	} else {
		defer response.Body.Close()
		responseData, _ := ioutil.ReadAll(response.Body)
		fmt.Println(string(responseData))
	}
}
```
Denna kod skickar en GET-begäran till webcode.me och skriver sedan ut svaret i konsolen.

## Djupdykning:
Att utföra HTTP-begäran i Go kräver inte särskilt mycket kod tack vare http-paketet som medföljer det standarda Go-biblioteket. Detta bibliotek och teknik för HTTP-begäran har funnits sedan de tidiga dagarna av webbprogrammering.

Det finns alternativ till Go:s inbyggda http-paket, som till exempel Go-http-client och httpwrapper, som erbjuder olika fördelar som ökad funktionalitet eller förenklad kodstruktur.

Vad gäller genomförandet utförs GET-begäran genom att använda http.Get funktionen och ta emot ett svar och eventuellt ett fel. Du bör alltid stänga svarkroppen när du är klar med den för att frigöra systemresurser.

## Se Även:
Vill du lära dig mer om HTTP-begäran? Kolla in dessa kopplingar!

1. Go's offentliga dokumentation på [net / http-paketet](https://golang.org/pkg/net/http/)
2. [“Go by Example: HTTP Clients”](https://gobyexample.com/http-clients) ger bra exempel att följa.
3. För att lära dig mer om HTTP i allmänhet, ta en titt på Mozillas [HTTP-översikt](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)).