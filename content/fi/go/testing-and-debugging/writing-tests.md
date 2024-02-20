---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:23.023718-07:00
description: "Testien kirjoittaminen Go-kielell\xE4 k\xE4sitt\xE4\xE4 pienien, hallittavien\
  \ koodip\xE4tkien luomisen, jotka varmistavat sovelluksesi toiminnallisuuden ja\u2026"
lastmod: 2024-02-19 22:05:14.969273
model: gpt-4-0125-preview
summary: "Testien kirjoittaminen Go-kielell\xE4 k\xE4sitt\xE4\xE4 pienien, hallittavien\
  \ koodip\xE4tkien luomisen, jotka varmistavat sovelluksesi toiminnallisuuden ja\u2026"
title: Testien kirjoittaminen
---

{{< edit_this_page >}}

## Mitä & Miksi?

Testien kirjoittaminen Go-kielellä käsittää pienien, hallittavien koodipätkien luomisen, jotka varmistavat sovelluksesi toiminnallisuuden ja käyttäytymisen. Ohjelmoijat kirjoittavat testejä varmistaakseen, että heidän koodinsa toimii odotetusti eri olosuhteissa, helpottaakseen uudelleenkirjoittamista ja auttaakseen estämään regressioita.

## Miten:

Go:ssa testit kirjoitetaan tyypillisesti samaan pakettiin testattavan koodin kanssa. Testeihin tarkoitetut tiedostot on nimetty `_test.go`-liitteellä. Testit ovat funktioita, jotka ottavat argumentiksi osoittimen testing.T-objektiin (`testing`-paketista), ja ne ilmaisevat epäonnistumisen kutsumalla metodeita, kuten `t.Fail()`, `t.Errorf()` jne.

Esimerkki yksinkertaisesta testistä `Add`-funktiolle, joka on määritelty `math.go`-tiedostossa:
```go
// math.go
package math

func Add(x, y int) int {
    return x + y
}
```

Testitiedosto `math_test.go`:
```go
package math

import "testing"

func TestAdd(t *testing.T) {
    tulos := Add(1, 2)
    odotettu := 3
    if tulos != odotettu {
        t.Errorf("Add(1, 2) = %d; halutaan %d", tulos, odotettu)
    }
}
```

Suorita testisi komennolla `go test` samassa hakemistossa testitiedostojesi kanssa. Esimerkkituloste, joka osoittaa testin läpimenon, näyttäisi samankaltaiselta:

```
PASS
ok      example.com/my/math 0.002s
```

Taulukkoon perustuvissa testeissä, jotka mahdollistavat tehokkaan testaamisen eri syöte- ja tulosteyhdistelmillä, määritä rakenne, joka sisältää testitapaukset:

```go
func TestAddTableDriven(t *testing.T) {
    var tests = []struct {
        x        int
        y        int
        odotettu int
    }{
        {1, 2, 3},
        {2, 3, 5},
        {-1, -2, -3},
    }

    for _, tt := range tests {
        testinimi := fmt.Sprintf("%d+%d", tt.x, tt.y)
        t.Run(testinimi, func(t *testing.T) {
            vastaus := Add(tt.x, tt.y)
            if vastaus != tt.odotettu {
                t.Errorf("saatiin %d, halutaan %d", vastaus, tt.odotettu)
            }
        })
    }
}
```

## Syväsukellus

Go-testauskehys, joka esiteltiin Go:n version 1 yhteydessä itse kielen ohella, on suunniteltu integroitumaan saumattomasti Go-työkaluketjuun, heijastaen Go:n painotusta yksinkertaisuuteen ja tehokkuuteen ohjelmistokehityksessä. Toisin kuin jotkut muissa kielissä käytetyt testauskehykset, jotka nojaavat ulkoisiin kirjastoihin tai monimutkaisiin asetuksiin, Go:n sisäänrakennettu `testing`-paketti tarjoaa suoraviivaisen tavan kirjoittaa ja suorittaa testejä.

Mielenkiintoinen näkökulma Go:n testauslähestymistapaan on konventio yli konfiguraation periaate, jonka se omaksuu, kuten tiedostonimen kaava (`_test.go`) ja standardikirjaston toimintojen käyttö ulkoisten riippuvuuksien sijaan. Tämä minimalistinen lähestymistapa kannustaa kehittäjiä kirjoittamaan testejä, koska aloituskynnys on matala. 

Vaikka Go:n sisäänrakennetut testausominaisuudet kattavat paljon, on olemassa skenaarioita, joissa kolmannen osapuolen työkalut tai kehykset saattavat tarjota enemmän toiminnallisuuksia, kuten mock-generointia, fuzz-testausta tai behavior-driven development (BDD) -tyylisiä testejä. Suositut kirjastot, kuten Testify tai GoMock, täydentävät Go:n standarditestauskykyjä tarjoamalla ekspressiivisempiä väitteitä tai mock-generointi ominaisuuksia, jotka voivat olla erityisen hyödyllisiä monimutkaisissa sovelluksissa, joissa on paljon riippuvuuksia.

Huolimatta näiden vaihtoehtojen olemassaolosta, Go:n standardi testauspaketti säilyy Go-testauksen kulmakivenä sen yksinkertaisuuden, suorituskyvyn ja tiiviin integraation kielen ja työkaluketjun kanssa vuoksi. Olipa kehittäjien valinta laajentaa sitä kolmannen osapuolen työkaluilla tai ei, Go-testauskehys tarjoaa vankan perustan koodin laadun ja luotettavuuden varmistamiseksi.
