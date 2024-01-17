---
title:                "Kirjoittaminen standardivirheeseen"
html_title:           "Go: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Kirjoittaminen standardivirheeseen on tapa, jolla ohjelmoijat voivat kirjoittaa virheitä tai ilmoituksia, jotka näkyvät komentorivillä tai komentokehotteella. Tämä on kätevä tapa ilmoittaa virheistä ja vianmäärityksen helpottamiseksi.

## Miten:
Go-kielellä voit kirjoittaa standardivirheeseen käyttämällä ```Go os.Stderr.Write([]byte("virheilmoitus"))``` -komentoa. Tämä kirjoittaa annetun virheilmoituksen standardivirheeseen. Voit myös käyttää ```Go fmt.Fprintf(os.Stderr, "Virhe: %v", err)``` -komentoa, joka kirjoittaa muotoillun virheilmoituksen standardivirheeseen.

## Syväsukellus:
Kirjoittaminen standardivirheeseen on yleinen tapa käsitellä ohjelmien virheitä ja ilmoituksia. Se vaatii vähemmän koodia kuin esimerkiksi tekstin tulostaminen näytölle tai tiedostoon ja tarjoaa selkeämmän tavan ilmoittaa virheistä. On myös mahdollista ohjata standardivirhe toiseen tiedostoon tai jopa piilottaa se kokonaan ohjelman suorituksen aikana.

## Katso myös:
-[Go-kielen viralliset dokumentaatiot standardivirheestä](https://golang.org/pkg/os/#pkg-variables)
-[Esimerkki ohjelmasta, joka käyttää standardivirheen kirjoittamista](https://play.golang.org/p/G1N4C3HzDvR)
-[Vinkkejä ohjelmien virheiden käsittelyyn](https://www.geeksforgeeks.org/error-handling-in-go/)