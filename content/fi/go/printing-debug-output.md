---
title:                "Tulostaminen debuggausnäkymä"
html_title:           "Go: Tulostaminen debuggausnäkymä"
simple_title:         "Tulostaminen debuggausnäkymä"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?
Tulostamalla debug outputia tarkoitetaan ohjelman tulostamista ohjelman suorituksen aikana, jotta löydettäisiin mahdollisia virheitä ja ongelmakohtia. Ohjelmoijat tekevät näin varmistaakseen, että ohjelma toimii odotetusti ja jotta he voivat helposti paikantaa mahdollisia virheitä.

## Miten:
```Go
fmt.Println("Debug viesti") // Tulostaa debug viestin konsoliin
```
Sample Output: 
`Debug viesti`

## Syvempi sukellus:
Tulostamalla debug outputia on ollut tärkeä osa ohjelmoijien työkaluja jo pitkään. Aiemmin debug output tulostettiin usein papereille tai tiedostoihin, mutta nykyään se tapahtuu yleensä konsoliin tai muuhun näyttöön. Vaihtoehtoisesti ohjelmoijat voivat käyttää esimerkiksi debugger-työkaluja, jotka auttavat löytämään ja korjaamaan virheitä.

## Katso myös:
- [Go:n virallinen dokumentaatio](https://golang.org/doc/)
- [Kehittäjän opas Go:hen](https://go.dev/learn/)
- [Blogikirjoitus Go:sta](https://blog.golang.org/)