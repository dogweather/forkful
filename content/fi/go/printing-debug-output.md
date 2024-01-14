---
title:    "Go: Virheenkorjaustulosteen tulostaminen"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmoijat käyttävät debug tulostusta ratkaistakseen ongelmia ohjelmassaan. Se tarjoaa tarkan kuvan siitä, mitä ohjelma tekee vaihe vaiheelta ja auttaa tunnistamaan mahdolliset virheet tai pitkäksi venyvät osat koodissa. 

## Kuinka

```Go
// Esimerkki debug tulostuksesta
fmt.Println("Ohjelma aloitetaan.")
// Ohjelman aloitustuloste: Ohjelma aloitetaan.
```

Debug tulostuksen käyttö Go-kielessä on yksinkertaista. Käytetään fmt-pakettia ja sen Print-funktioita tulostamaan haluttu tieto konsoliin. Tämä voidaan tehdä koodin eri osissa, jotta saadaan selkeämpi kuva ohjelman toiminnasta. Tarvittaessa voidaan käyttää myös log-pakettia, joka tarjoaa lisää vaihtoehtoja tulostuksen hallintaan.

## Syvempi syventyminen

Debug tulostus voi myös olla hyödyllistä jäljittäessä virheitä, joita ei välttämättä näy muuten. Esimerkiksi jos ohjelma ei tulosta haluttua tulosta, voidaan tarkastella koodin osia ja lisätä tarvittaessa debug tulostuksia, jotta nähdään missä vaiheessa virhe tapahtuu. Myös muiden tietojen, kuten muuttujien arvojen, tulostaminen voi auttaa ratkaisemaan ongelmia.

## Katso myös

- [The Go Blog: Debugging with Print Statements](https://blog.golang.org/using-go)
- [Go Documentation: fmt Package](https://golang.org/pkg/fmt/)
- [Go Documentation: log Package](https://golang.org/pkg/log/)