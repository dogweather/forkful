---
title:                "Swift: Skrive til standard feil"
simple_title:         "Skrive til standard feil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger når vi koder, vil vi at programmet skal vise en melding som informerer oss om noe som skjer, selv om vi ikke har lagt det inn i koden vår. For dette formålet, kan vi bruke standard error output. Det er også nyttig å bruke når du vil vise feilmeldinger i stedet for å krasje programmet. Så hvorfor skrive til standard error output? Fordi det kan gi oss informasjon vi trenger for å forbedre våre programmeringsferdigheter og feilsøke problemer.

## Hvordan gjør man det

For å skrive til standard error output i Swift, trenger vi å bruke "```fprint```" funksjonen og spesifisere "stderr" som parameter. Her er et eksempel:

```Swift
fprint(stderr, "Dette er en feilmelding")
```

Når dette kjøres, vil det skrive "Dette er en feilmelding" til standard error output.

## Dypdykk

Når vi skriver til standard error output, er det viktig å huske på følgende punkter:

- Standard error output er ofte brukt for å vise feilmeldinger i stedet for å krasje programmet. Dette hjelper oss med å identifisere og løse problemer raskt.
- Det er også nyttig å bruke "```Assertion```" for å sikre at programmet stopper og viser feilmeldinger hvis noe uventet skjer.
- Hvis du vil redirektere standard error output til en fil, kan du bruke "```freopen```" funksjonen og spesifisere filnavn og "w+" for å skrive og opprette en ny fil hvis den ikke finnes.

## Se også

- [Feilsøking og debugging i Swift](https://developer.apple.com/videos/play/wwdc2018/402/)
- [Standard Library Reference - fprint](https://developer.apple.com/documentation/swift/1641343-fprint)
- [Swift.org - Error handling](https://swift.org/documentation/#errorhandling)