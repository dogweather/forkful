---
title:                "Een HTTP-verzoek verzenden"
aliases:
- nl/go/sending-an-http-request.md
date:                  2024-02-03T18:08:42.473950-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/sending-an-http-request.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het verzenden van een HTTP-verzoek houdt in dat er vanuit je Go-applicatie een oproep wordt gedaan naar een webserver, API of een andere op HTTP-gebaseerde dienst. Programmeurs doen dit om te communiceren met webbronnen, gegevens op te halen, formulieren in te dienen of te communiceren met andere services op het internet.

## Hoe te:

In Go, het verzenden van een HTTP-verzoek en het afhandelen van de response, betekent gebruik maken van het `net/http` pakket. Hier is een stap-voor-stap voorbeeld dat laat zien hoe je een eenvoudige GET-aanvraag stuurt en de response leest:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "net/http"
)

func main() {
    // Definieer de URL van de bron
    url := "http://example.com"

    // Gebruik http.Get om het GET-verzoek te verzenden
    resp, err := http.Get(url)
    if err != nil {
        log.Fatal(err)
    }
    // Sluit het response body wanneer de functie eindigt
    defer resp.Body.Close()

    // Lees het response body
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        log.Fatal(err)
    }

    // Zet het response body om in een string en druk het af
    fmt.Println(string(body))
}
```

Voorbeelduitvoer (ingekort voor de beknopte vorm):
```
<!doctype html>
<html>
<head>
    <title>Voorbeeld Domein</title>
...
</html>
```

Om een POST-verzoek met formuliergegevens te sturen, kun je `http.PostForm` gebruiken:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
    "net/url"
)

func main() {
    // Definieer de URL en de formuliergegevens
    url := "http://example.com/form"
    data := url.Values{}
    data.Set("key", "value")

    // Verzend het POST-verzoek met formuliergegevens
    resp, err := http.PostForm(url, data)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    // Lees en druk de response af
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }

    fmt.Println(string(body))
}
```

## Diepgaande duik

Het `net/http` pakket in Go biedt een krachtige en flexibele manier om te interageren met HTTP-servers. Het ontwerp weerspiegelt de nadruk van Go op eenvoud, efficiëntie en robuustheid. Oorspronkelijk vereisten functionaliteiten zoals het afhandelen van JSON of XML payloads het handmatig samenstellen van het request body en het instellen van geschikte headers. Naarmate Go zich ontwikkelde, heeft de community hoger-niveau pakketten ontwikkeld die deze taken verder vereenvoudigen, zoals `gorilla/mux` voor routing en `gjson` voor JSON-manipulatie.

Een opmerkelijk aspect van de HTTP-client van Go is het gebruik van interfaces en structs, zoals `http.Client` en `http.Request`, die uitgebreide aanpassing en testen mogelijk maken. Je kunt bijvoorbeeld de `http.Client` wijzigen om verzoeken te laten verlopen of verbindingen levend te houden voor de prestatie.

Een overwogen alternatief voor eenvoudigere HTTP-interacties is het gebruik van bibliotheken van derden zoals "Resty" of "Gentleman." Deze pakketten bieden een hoger abstractieniveau voor HTTP-verzoeken, waardoor veelvoorkomende taken bondiger worden. Het begrijpen en gebruiken van het onderliggende `net/http` pakket is echter cruciaal voor het omgaan met complexere of unieke HTTP-interactiescenario's, en biedt een fundament waarop de gelijktijdigheidsfuncties van Go en de krachtige standaardbibliotheek volledig kunnen worden benut.
