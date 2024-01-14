---
title:                "Go: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa merkkijonon pieniksi kirjaimiksi? Tämä on erittäin hyödyllinen tekniikka esimerkiksi tekstianalyysissä tai vertailussa, kun haluat poistaa kokoelmaan liittyviä isotähtimiä.

## Miten

```Go
functoLowerCase(s string)string {
	returnstrings.ToLower(s)
}

funcmain() {
	// Alkuperäinen merkkijono
	s:= "Tämä on MATALAISOA testi"
	// Tulostaa "tämä on matalavisa testi"
	fmt.Println(toLowerCase(s))
}
```
```Tämäonmatalavisa testi```

## Syvällinen sukellus

Golangilla on sisäänrakennettu funktio ```ToLower```, joka muuttaa merkkijonon kaikki isot kirjaimet pieniksi. Huomaa, että tällä funktiolla on joitain rajoituksia kielten kuten turkkilaisen ja azteekin tapauksessa. Tämä johtuu siitä, että näissä kielissä on määrättyjä kirjaimia, jotka voivat muuttua eri tavoin kirjoitetuiksi. On suositeltavaa käyttää Unicode-yhteensopivaa ```ToLower``` -funktiota näissä tapauksissa.

## Katso myös

- [Go:n viralliset dokumentaatiot](https://golang.org/pkg/strings/#ToLower)
- [Go:n merkkijonojen käsittely](https://gobyexample.com/strings)
- [UnitConverter: Merkkijonon muuntaminen pieniksi kirjaimiksi](https://www.sohamkamani.com/golang/converting-string-to-lowercase/)