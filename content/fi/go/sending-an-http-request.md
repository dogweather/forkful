---
title:                "Go: Lähettää http-pyyntö."
simple_title:         "Lähettää http-pyyntö."
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Miksi

On monia syitä, miksi haluaisit lähettää HTTP-pyynnön ohjelmoinnin yhteydessä. Se voi olla osa tiedonsiirtoa kolmansien osapuolten palveluiden kanssa, kutsua API-palveluita tai hakata verkkosivustoja.

# Kuinka tehdä

Lähettäminen HTTP-pyyntöön Go-kielellä on yksinkertaista ja tehokasta. Käytä vain sisäänrakennettua "net/http" -pakettia ja "http.NewRequest" -funktiota. Alla on esimerkki yksinkertaisesta GET-pyynnöstä ja sen vastauksen tulostamisesta konsolille.

```Go
package main

import (
    "fmt"
    "net/http"
)

func main() {
    url := "https://www.example.com"
    req, err := http.NewRequest("GET", url, nil)
    if err != nil {
        fmt.Println("Virhe luotaessa pyyntöä:", err)
    }
    client := http.Client{}
    resp, err := client.Do(req)
    if err != nil {
        fmt.Println("Virhe lähettäessä pyyntöä:", err)
    }
    fmt.Println("Vastauksen statuskoodi:", resp.StatusCode)
    defer resp.Body.Close()
}
```

## Syöte
```
Vastauksen statuskoodi: 200
```

# Syvemmälle

HTTP-pyynnöt koostuvat useista eri osista, kuten otsikoista ja kehosta. Voit asettaa näitä osia käyttämällä "http." -pakettia. Alla on esimerkki POST-pyynnöstä, jossa lähetetään JSON-muotoista tietoa ja asetetaan tarvittavat otsikot.

```Go
package main

import (
    "fmt"
    "bytes"
    "net/http"
)

func main() {
    url := "https://api.example.com"
    data := []byte(`{"nimi": "Matti Meikäläinen", "ika": 25}`)
    req, err := http.NewRequest("POST", url, bytes.NewBuffer(data))
    req.Header.Set("Content-Type", "application/json")
    req.Header.Set("Authorization", "12345")
    client := http.Client{}
    resp, err := client.Do(req)
    if err != nil {
        fmt.Println("Virhe lähettäessä pyyntöä:", err)
    }
    fmt.Println("Vastauksen statuskoodi:", resp.StatusCode)
    defer resp.Body.Close()
}
```

## Syöte
```
Vastauksen statuskoodi: 201
```

# Katso myös

- [Go:n virallinen dokumentaatio HTTP-pyynnöistä](https://golang.org/pkg/net/http/)
- [Esimerkkejä HTTP-pyyntöjen lähettämisestä Go:lla](https://tutorialedge.net/golang/http-post-get-request-tutorial/)
- [HTTP-pyyntöjen virheitä ja ratkaisuja Go:lla](https://blog.golang.org/error-handling-and-go)