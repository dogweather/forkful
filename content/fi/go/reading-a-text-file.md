---
title:                "Go: Tiedostotekstin lukeminen"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Lukeminen ja kirjoittaminen tiedostoihin on olennainen osa lähes jokaista ohjelmointiprojektia. Tekstitiedostojen lukeminen erityisesti on välttämätöntä, sillä ne sisältävät arvokasta tietoa, jota ohjelman on käsiteltävä. Tässä blogitekstissä käsittelemme lukemista Go-ohjelmointikielellä ja annamme esimerkkejä, jotka auttavat sinua ymmärtämään, miksi se on niin tärkeää.

## Kuinka

Go-kielellä on helppo lukea tekstitiedostoja. Alla olevassa koodiesimerkissä näytämme, kuinka voit avata tiedoston, lukea sen sisällön ja tulostaa sen konsoliin.

```
tiedosto, err := os.Open("teksti.txt")
if err != nil {
   fmt.Println("Tiedoston avaaminen epäonnistui:", err)
   
}
defer tiedosto.Close()

skanneri := bufio.NewScanner(tiedosto)
for skanneri.Scan() {
   rivi := skanneri.Text()
   fmt.Println(rivi)
}
```

Yllä olevassa koodiesimerkissä ensin avataan tiedosto käyttäen `os.Open` -funktiota. Jos tiedoston avaaminen epäonnistuu, koodissa käsitellään virhe `err`-muuttujan avulla. Tämän jälkeen tiedosto suljetaan `defer`-lauseen avulla, varmistaen että tiedosto suljetaan oikein lopuksi.

Seuraavaksi luodaan skanneri `bufio.NewScanner` -funktion avulla ja käytetään `Scan` metodia lukeaksemme tiedoston rivin kerrallaan. Lopuksi kukin rivi tulostetaan konsoliin `Text` -metodin avulla.

Voit myös käyttää `bufio.Scanner`-tyyppiä käsittelemään tiedostoa omalla tavallasi. Alla olevassa koodiesimerkissä käytetään `scanner.bytes()`-metodia ja tulostetaan tiedoston sisältö heksadesimaalimuodossa. Voit kokeilla erilaisia tapoja käsitellä skanneria ja tiedoston sisältöä.

```
tiedosto, err := os.Open("teksti.txt")
if err != nil {
   fmt.Println("Tiedoston avaaminen epäonnistui:", err)
   
}
defer tiedosto.Close()

skanneri := bufio.NewScanner(tiedosto)
skanneri.Split(bufio.ScanBytes)
for skanneri.Scan() {
   byte := skanneri.Bytes()
   fmt.Printf("%x ", byte)
}
```

## Syväsyvennys

Kun luet tekstitiedostoja Go-kielellä, on tärkeää ymmärtää, että tiedosto koostuu tavuista (bytes) ja merkeistä (characters). Yksi tavu voi sisältää yhden tai useamman merkin, riippuen käytetystä tekstikoodauksesta. Tämän vuoksi on tärkeää valita oikea tapa käsitellä tiedostoa sen sisällön mukaan.

Go-kielessä on myös mahdollista lukea ja käsitellä CSV-tiedostoja `encoding/csv` -kirjaston avulla. Tämä on hyödyllistä esimerkiksi tietokantojen tai taulukoiden lukemiseen ja käsittelyyn ohjelmassa.

## Katso myös

- https://golang.org/pkg/os/#Open
- https://golang.org/pkg/bufio/#NewScanner
- https://golang.org/pkg/bufio/#Scanner.Bytes
- https://golang.org/pkg/encoding/csv/