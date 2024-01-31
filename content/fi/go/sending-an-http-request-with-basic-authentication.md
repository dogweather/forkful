---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnilla"
date:                  2024-01-20T18:02:10.900202-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Lähetämme HTTP-pyynnön perusautentikaatiolla lisäämään käyttäjätunnus ja salasana pyyntöön. Tätä tehdään päästäksemme käsiksi suojattuihin resursseihin.

## How to: - Miten:
```Go
package main

import (
    "encoding/base64"
    "fmt"
    "net/http"
    "io/ioutil"
)

func main() {
    client := &http.Client{}
    req, err := http.NewRequest("GET", "https://some-protected-resource.com", nil)
    if err != nil {
        panic(err)
    }

    // Lisää perusautentikaatio-header
    auth := base64.StdEncoding.EncodeToString([]byte("käyttäjänimi:salasana"))
    req.Header.Add("Authorization", "Basic "+auth)

    resp, err := client.Do(req)
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
Jos kirjaudut sisään onnistuneesti, vastaukseksi saat palvelimen resurssin sisällön.

## Deep Dive - Syväsukellus:
HTTP-perusautentikaatio on yksinkertainen autentikaatioprotokolla, joka vaatii käyttäjätunnusta ja salasanaa. Se ei ole turvallisin menetelmä, sillä tunnukset lähetetään Base64-koodattuna, mikä ei ole turvallista salaamatonta yhteyttä käytettäessä. Parempi vaihtoehto on käyttää OAuth, kerrosturvaprotokollia tai HTTPS-yhteyttä, joka suojaa tietoja salauksella.

Perusautentikaatio oli yksi ensimmäisistä webbikäytön autentikointimenetelmistä, ja se on edelleen osa HTTP-standardia (RFC 7617). Sen käyttö on nopeaa, jos tarvitaan yksinkertaista suojaa ja kehitysresurssit ovat rajalliset.

Koodiesimerkissämme käytämme standardi `http` kirjastoa. Autentikaatioheader luodaan Base64-koodaamalla käyttäjänimi ja salasana. Muista aina käyttää `https`, kun lähetät arkaluonteisia tietoja.

## See Also - Katso Myös:
- Go’s http package documentation: https://pkg.go.dev/net/http
- HTTP Basic Authentication Standard (RFC 7617): https://tools.ietf.org/html/rfc7617
- Go-mallit turvallisempiin HTTP-pyyntöihin: https://github.com/golang/go/wiki/Authentication
- HTTPS ja Go: https://blog.golang.org/http-tracing
