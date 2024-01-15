---
title:                "Å sende en http-forespørsel"
html_title:           "Swift: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

HTTP-forespørsler er et viktig aspekt av webutvikling, som tillater kommunikasjon mellom klienter og servere. Ved å lære å sende HTTP-forespørsler i Swift, kan du lage applikasjoner som effektivt samhandler med forskjellige nettjenester og henter data som er nødvendig for dine behov.

## Slik gjør du det

Før du begynner å sende HTTP-forespørsler, må du først importere Foundation-rammeverket i prosjektet ditt. Dette er nødvendig for å bruke de innebygde klassene for å håndtere nettverksforespørsler i Swift. Dette gjøres ved å legge til følgende linje øverst i filen din:

```Swift
import Foundation
```

Nå kan du opprette en URL-objekt for å angi hvor forespørselen skal sendes. Dette kan gjøres ved hjelp av URL-klassen i Foundation-rammeverket. For eksempel, hvis vi skal sende en GET-forespørsel til Google sin søk API, kan URL-objektet se slik ut:

```Swift
let url = URL(string: "https://www.googleapis.com/books/v1/volumes?q=Swift")!
```

Merk at vi bruker "!" -symbolet for å sikre at URL-en alltid inneholder en gyldig verdi.

Deretter oppretter vi en URLRequest-objekt med vår URL og angir forespørselsmetoden (GET i dette tilfellet) ved hjelp av URLRequest-klassen:

```Swift
var request = URLRequest(url: url)
request.httpMethod = "GET"
```

Nå kan vi sende forespørselen ved hjelp av URLSession, som er en del av Foundation-rammeverket. Dette gjøres ved å opprette et URLSessionDataTask-objekt og deretter bruke "resume" -metoden for å starte forespørselen:

```Swift
let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
    if let error = error {
        print("Error: \(error)")
    } else if let response = response as? HTTPURLResponse, let data = data {
        // Behandle data og svar her
    }
}
task.resume()
```

Når forespørselen er fullført, vil vi motta et svar og eventuelle data som er returnert av serveren. Dette kan behandles i "data, response, error" -blokken i vår dataoppgave.

## Dypdykk

Nå som du har lært hvordan du kan sende en HTTP-forespørsel i Swift, kan det være nyttig å vite mer om forskjellige typer forespørsler og hvordan de kan konfigureres. URLRequest-objektet har et bredt utvalg av egenskaper som kan tilpasses, for eksempel å legge til en HTTP-kropp, angi forespørselshodere og mer.

I tillegg til GET-metoden, støtter URLRequest også andre metoder som POST, PUT, DELETE og mer. Disse kan spesifiseres ved å endre "httpMethod" -egenskapen til forespørselen.

Det er også viktig å være oppmerksom på sikkerheten rundt å sende HTTP-forespørsler. Det er viktig å sørge for at sensitiv informasjon, som passord eller brukernavn, ikke sendes i klartekst gjennom en HTTP-forespørsel. I stedet bør du bruke HTTPS, som krypterer dataene dine for å sikre privatliv og sikkerhet.

## Se også

1. [Apple dokumentasjon for HTTP-forespørsler i Swift](https://developer.apple.com/documentation/foundation/url_loading_system)
2. [Swift-ressurser fra Hacking with Swift](https://www.hackingwithswift.com/)
3. [Stack Overflow for å stille spørsmål og søke etter hjelp](https://stackoverflow.com/)