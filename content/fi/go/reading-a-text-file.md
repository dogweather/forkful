---
title:                "Tekstitiedoston lukeminen"
date:                  2024-01-20T17:54:28.779813-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Lukemalla tekstitiedostoja saat dataa ohjelmaasi. Se on perusjuttu; tehdään, koska meidän pitää käsitellä ja analysoida informaatiota.

## How to: (Kuinka tehdä:)
```Go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
)

func main() {
    file, err := os.Open("esimerkki.txt")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }

    if err := scanner.Err(); err != nil {
        log.Fatal(err)
    }
}
```
Output:
```
Tässä on esimerkin ensimmäinen rivi.
Ja toinen.
```

## Deep Dive (Syvä sukellus)
Tiedostonlukuun liittyy historiaa. UNIX-aikakaudella kaikki oli tiedostoa, myös teksti. Tämän vuoksi Go:n tiedostonkäsittely on uniikki erityisesti io ja os pakettien välillä. Vaihtoehtoja lukemiseen on monia: ioutil.ReadAll (vanha tapa), os paketti (uusi tapa), tai bufio lukijat. bufio on hyvä isommille tiedostoille, kosä se ei lataa koko tiedostoa muistiin kerralla. Implementation-wise, `defer` käskyä käytetään tiedoston varmaan sulkemiseen sen jälkeen, kun kaikki toiminnot on suoritettu.

## See Also (Katso myös)
- Go by Example: Reading Files: https://gobyexample.com/reading-files
- The Go Programming Language Specification: https://golang.org/ref/spec#Package_os
- Go Blog – Defer, Panic, and Recover: https://blog.golang.org/defer-panic-and-recover
