---
aliases:
- /no/swift/removing-quotes-from-a-string/
date: 2024-01-26 03:42:12.401213-07:00
description: "\xC5 fjerne anf\xF8rselstegn fra en streng betyr \xE5 strippe vekk eventuelle\
  \ anf\xF8rselstegn som omslutter innholdet. Dette gj\xF8res for \xE5 sanere inndata,\
  \ forberede\u2026"
lastmod: 2024-02-18 23:08:54.257656
model: gpt-4-0125-preview
summary: "\xC5 fjerne anf\xF8rselstegn fra en streng betyr \xE5 strippe vekk eventuelle\
  \ anf\xF8rselstegn som omslutter innholdet. Dette gj\xF8res for \xE5 sanere inndata,\
  \ forberede\u2026"
title: "Fjerne anf\xF8rselstegn fra en streng"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å fjerne anførselstegn fra en streng betyr å strippe vekk eventuelle anførselstegn som omslutter innholdet. Dette gjøres for å sanere inndata, forberede data for lagring, eller for å kvitte seg med unødvendig tekstformatering som kan forstyrre databehandlingen.

## Hvordan:

Swift lar deg takle jobben med å fjerne anførselstegn ganske hendig. Her er et kjapt eksempel ved bruk av `replacingOccurrences(of:with:)`, som gjør akkurat det det høres ut som - bytter ut biter av tekst med noe annet, eller ingenting i det hele tatt.

```swift
var quotedString = "\"Dette er en 'sitert' streng.\""
let unquotedString = quotedString.replacingOccurrences(of: "\"", with: "")
print(unquotedString) // Dette er en 'sitert' streng.

// Håndtere enkle anførselstegn? Bare endre søkeordet.
quotedString = "'Her er et annet eksempel.'"
let singleQuoteRemoved = quotedString.replacingOccurrences(of: "'", with: "")
print(singleQuoteRemoved) // Her er et annet eksempel.
```

Utdata vil være strenger uten anførselstegn, klare for hva enn du har planlagt videre.

## Dypdykk

Vi har "renset opp" strenger som disse siden programmeringens morgen. I de tidlige dagene handlet det mer om å spare dyrebar minne og unngå syntaksfeil ved behandling av inndata. Hurtig frem til i dag, og det handler om god datahygiene – spesielt når man håndterer JSON eller forbereder strenger for databasarbeid. Et feilplassert anførselstegn kan kaste en skiftenøkkel inn i SQL-forespørsler raskere enn du kan si "syntaksfeil".

Alternativer? Vel, hvis du finner `replacingOccurrences(of:with:)` litt for vanilje, kan du utforske regulære uttrykk for mer komplekse mønstre eller når du ønsker å fjerne anførselstegn kun i bestemte posisjoner. Swifts `NSRegularExpression`-klasse er din venn her. Men husk, regex kan være et tveegget sverd – kraftfullt, men noen ganger overkill.

Implementasjonsmessig er `replacingOccurrences(of:with:)` en metode som tilbys av `String` i Swift, som internt kaller mer komplekse strengmanipuleringsfunksjoner som håndterer Unicode og andre intrikatesser ved moderne tekstbehandling. Det er en av de "enkelt på overflaten, komplekst under panseret"-tilbudene som Swift håndterer slik at du slipper.

## Se også

For mer om strengmanipulasjoner i Swift:

- Swift Programmeringsspråk (Strenger og Tegn): [Swift.org Dokumentasjon](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- NSRegularExpression: [Apple Utviklerdokumentasjon](https://developer.apple.com/documentation/foundation/nsregularexpression)

Og hvis du nå er nysgjerrig på regulære uttrykk og ønsker å teste dine mønstre:

- Regex101: [Regex Tester og Debugger](https://regex101.com)
