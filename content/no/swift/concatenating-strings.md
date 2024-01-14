---
title:    "Swift: Sammenslåing av strenger"
keywords: ["Swift"]
---

{{< edit_this_page >}}

##Hvorfor

Så hvorfor ville noen ønske å sette sammen strenger (concatenate strings)? Vel, dette er en vanlig oppgave i programmering når man trenger å kombinere flere tekststykker for å lage en lengre streng. Dette kan være nyttig når man skal lage en beskjed eller e-post, eller når man trenger å formatere data på en spesifikk måte. Ved å lære å sette sammen strenger i Swift, kan du gjøre programmeringen din mer effektiv og praktisk.

##Slik gjør du det

La oss se på et enkelt eksempel ved hjelp av Swift-kodeblokker. Vi skal sette sammen navn og etternavn til en fullstendig navnsammenstilling. Først må vi opprette to variabler som inneholder navnet og etternavnet:

```Swift
let navn = "Jenny"
let etternavn = "Johansen"
```

Deretter kan vi bruke operatoren "+" (pluss) til å kombinere disse to tekstene i en ny variabel som heter "fulltNavn":

```Swift
let fulltNavn = navn + " " + etternavn
```

I dette eksempelet har vi også inkludert et mellomrom mellom navn og etternavn, slik at det fullstendige navnet blir formatert riktig.

Etter å ha kjørt denne koden, vil verdien av "fulltNavn" være "Jenny Johansen". Kodeblokkene ovenfor viser bare et enkelt eksempel, men det er mange forskjellige måter å sette sammen strenger på, avhengig av behovene dine. Du kan for eksempel også bruke en "append" -funksjon eller bruke variabler sammen med en streng for å oppnå ønsket resultat.

##Dypdykk

Når det gjelder å sette sammen strenger i Swift, er det noen viktige ting å huske på. For det første, når en streng blir satt sammen med en annen streng eller variabel, blir begge verdiene konvertert til strenger før de slås sammen. Dette betyr at du kan kombinere forskjellige datatyper (som tall og strenger) ved å konvertere dem til strenger først.

En annen ting å huske på er at hvis du setter sammen mange lange strenger, kan dette ha en negativ innvirkning på ytelsen til koden din. Dette er fordi å sette sammen strenger er en ressurskrevende operasjon, spesielt når det gjøres mange ganger i en løkke. Derfor kan det være lurt å begrense bruken av strykning til de tilfellene der det er absolutt nødvendig.

##Se også

* [Swift Offisiell Nettside](https://swift.org/)
* [Hvordan bruke variabler i Swift](https://www.appcoda.com/swift-variables/)
* [Swift String Dokumentasjon](https://developer.apple.com/documentation/swift/string)