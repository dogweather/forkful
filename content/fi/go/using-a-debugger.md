---
title:                "Debuggerin käyttö"
date:                  2024-01-26T03:50:20.783561-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debuggerin käyttö"

category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/using-a-debugger.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Debuggerin käyttäminen on kuin GPS:n käyttö koodiviidakossa; se ohjaa sinut ongelman lähteelle. Ohjelmoijat käyttävät debuggereita koodinsa läpikäymiseen askel askeleelta, muuttujien tarkasteluun ja virtauksen ymmärtämiseen, mikä helpottaa virheiden löytämistä ja suorituskyvyn optimointia.

## Kuinka:
Go:lla on sisäänrakennettu työkalu debuggaukseen nimeltä Delve (`dlv`). Aloittaaksesi asenna Delve, kirjoita yksinkertainen Go-ohjelma ja aja se debuggerin läpi.

```Go
// Ensin, asenna Delve
// go get -u github.com/go-delve/delve/cmd/dlv

// Esimerkki Go-ohjelmasta, tallenna nimellä main.go
package main

import "fmt"

func main() {
    message := "Debugataan Delve:llä!"
    fmt.Println(message)
}

// Aja ohjelmasi Delve:llä
// dlv debug

// Joitakin perus Delve-komentoja:
// (dlv) break main.main // asettaa keskeytyskohdan main-funktioon
// (dlv) continue // jatkaa suorittamista keskeytyskohtaan tai ohjelman loppuun
// (dlv) step // yksittäinen askel ohjelman läpi
// (dlv) print message // tulostaa muuttujan 'message' nykyisen arvon
// (dlv) quit // poistu Delve:stä
```

`dlv debug` -komennon ajaminen aloittaa debuggausistunnon. Kun olet asettanut keskeytyskohdan, voit astua ohjelmasi läpi ja nähdä, mitä sen sisällä tapahtuu.

## Syväsukellus
Historiallisesti Go-ohjelmoijat ovat käyttäneet useita työkaluja debuggaukseen, kuten GDB (GNU Debugger), mutta ovat kohdanneet haasteita, koska GDB ei ollut räätälöity Go:n ajonaikaiselle ympäristölle ja gorutiineille. Delve tuli pelastajaksi tarjoamalla paremman tuen Go:n uniikeille ominaisuuksille.

Delvelle on olemassa vaihtoehtoja, kuten `go-dbg`, ja jopa integroitu debugger-tuki IDE:ssä, kuten Visual Studio Code ja GoLand, jotka kietoutuvat Delve:n ympärille tarjoten käyttäjäystävällisemmän kokemuksen.

Toteutuspuolella Delve toimii käyttäen `runtime` ja `debug/gosym` paketteja, muun muassa, päästäkseen käsiksi ja tulkatakseen Go-ohjelman symboleja ja ajonaikaista tietoa. Sitä päivitetään jatkuvasti pysymään ajan tasalla uusien kielen ominaisuuksien ja versioiden kanssa.

## Katso Myös
- Delven virallinen repo: https://github.com/go-delve/delve
- Go Debugger -opas Go-tiimiltä: https://golang.org/doc/gdb
- Visual Studio Code Go-debuggaus: https://code.visualstudio.com/docs/languages/go#_debugging
