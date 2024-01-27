---
title:                "JSON-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Arduino: JSON-tiedostojen käsittely"
simple_title:         "JSON-tiedostojen käsittely"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON on syntyjään JavaScriptista, mutta siitä on tullut universaali dataformaatti. Go-ohjelmoijat käsittelevät JSONia, koska se on nettirajapintojen ja konfiguraatiotiedostojen kieli.

## How to:
Go tarjoaa `encoding/json` kirjaston JSONin käsittelyyn. Esimerkki lukee JSONia ja kirjoittaa sitä:

```Go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

// Määritellään structura, joka vastaa JSON-dataa
type Henkilo struct {
    Nimi  string `json:"nimi"`
    Ikä   int    `json:"ikä"`
    Email string `json:"email"`
}

func main() {
    // JSON-muotoinen merkkijono
    jsonData := `{"nimi": "Matti Meikäläinen", "ikä": 30, "email": "matti@example.com"}`

    // Deserialisoidaan JSON Henkilo-structiin
    var h Henkilo
    err := json.Unmarshal([]byte(jsonData), &h)
    if err != nil {
        log.Fatalf("JSONin lukeminen epäonnistui: %v", err)
    }
    fmt.Printf("Nimi: %s\nIkä: %d\nEmail: %s\n", h.Nimi, h.Ikä, h.Email)

    // Serialisoidaan Henkilo-structi takaisin JSON-muotoon
    uusiJSON, err := json.Marshal(h)
    if err != nil {
        log.Fatalf("JSONin kirjoittaminen epäonnistui: %v", err)
    }
    fmt.Println(string(uusiJSON))
}
```

Sample output:

```
Nimi: Matti Meikäläinen
Ikä: 30
Email: matti@example.com
{"nimi":"Matti Meikäläinen","ikä":30,"email":"matti@example.com"}
```

## Deep Dive
JSON syntyi 2000-luvulla nopeaksi datanvaihtoformaatiksi. XML oli vaihtoehtona, mutta JSON voitti keveytensä ansiosta. Go:n JSON-tuki on kattava: se sisältää automaattisen serialisoinnin ja deserialisoinnin, mutta voi olla hitaampi isossa datassa. Vaihtoehtoina on nopeampia kirjastoja kuten `json-iterator/go`.

## See Also
- Go `encoding/json` paketin dokumentaatio: https://pkg.go.dev/encoding/json
- JSONin virallinen sivusto: https://www.json.org/json-fi.html
- `json-iterator/go` GitHubissa: https://github.com/json-iterator/go
- Go blogi, JSONin käsittelystä: https://blog.golang.org/json
