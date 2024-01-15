---
title:                "Lähetetään http-pyyntö perusautentikoinnilla"
html_title:           "Go: Lähetetään http-pyyntö perusautentikoinnilla"
simple_title:         "Lähetetään http-pyyntö perusautentikoinnilla"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyyntöjen lähettäminen perusautentikaation avulla on yleinen tapa tietojen lähettämiseen ja vastaanottamiseen tietoverkoissa. Tämän menetelmän avulla voit luoda turvallisen yhteyden palvelimelle ja varmistaa, että vain valtuutetut käyttäjät voivat käyttää tietoja. 

## Miten

```Go
package main

import (
    "fmt"
    "net/http"
    "encoding/base64"
)

func main() {
    // Luodaan HTTP-pyyntö osoitteeseen "example.com"
    req, err := http.NewRequest("GET", "https://www.example.com", nil)
    if err != nil {
        fmt.Println(err)
        return
    }

    // Lisätään "Authorization" header pyyntöön, jossa on käyttäjätunnus ja salasana 
    username := "käyttäjätunnus"
    password := "salasana"
    auth := username + ":" + password
    base64Auth := base64.StdEncoding.EncodeToString([]byte(auth))
    req.Header.Set("Authorization", "Basic " + base64Auth)

    // Lähetetään pyyntö
    resp, err := http.DefaultClient.Do(req)
    if err != nil {
        fmt.Println(err)
        return
    }
    defer resp.Body.Close()

    // Tulostetaan vastauskoodi ja mahdollinen virheilmoitus
    fmt.Println("Vastauskoodi:", resp.Status)
    fmt.Println("Virheilmoitus:", resp.StatusText)
}
```

**Tulostus:**

```
Statuskoodi: 200 OK
Virheilmoitus: OK
```

## Syvempi sukellus

Perusautentikaatio toimii lähettämällä käyttäjätunnus ja salasana HTTP-pyynnön "Authorization" header-kentässä. Tällöin käyttäjätunnus ja salasana ovat Base64-merkistössä, joten tietoja ei voi lukea selkeänä tekstinä. Palvelin tarkistaa pyynnön "Authorization" kentän ja varmistaa, että se vastaa tallennettuja käyttäjätunnuksia ja salasanoja. Mikäli pyyntö vastaa, palvelin lähettää vastauksen tietojen tai pyydetyn toiminnon mukaisesti.

## Katso myös

- [https://golang.org/pkg/net/http/#Request](https://golang.org/pkg/net/http/#Request)
- [https://golang.org/pkg/encoding/base64/](https://golang.org/pkg/encoding/base64/)