---
title:                "Stort skrive ut en tekststreng"
html_title:           "Swift: Stort skrive ut en tekststreng"
simple_title:         "Stort skrive ut en tekststreng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kapitalisere en streng, eller gjøre den om til stor bokstav, er en vanlig oppgave i programmering. Dette kan være nyttig når man ønsker å fremheve visse ord eller setninger i en tekst, eller når man trenger å følge visse konvensjoner i en kodebase. Å kunne kapitalisere en streng er en grunnleggende ferdighet som vil være nyttig for alle som lærer Swift.

## Slik gjør du det

For å kapitalisere en streng i Swift, kan du bruke metoden `.uppercased()`. Dette vil gjøre alle bokstavene i strengen til store bokstaver.

```Swift
let minStreng = "hallo verden"
print(minStreng.uppercased())

// Output: HALLO VERDEN
```

Du kan også bruke metoden `.capitalized` for å bare gjøre første bokstav i hver setning til stor bokstav.

```Swift
let minStreng = "dette er en setning. og dette er en annen."
print(minStreng.capitalized)

// Output: Dette er en setning. Og dette er en annen.
```

Hvis du trenger å kun gjøre første bokstav i hele strengen til stor bokstav, kan du bruke metoden `.firstCapitalized()`.

```Swift
let minStreng = "swift er gøy å lære!"
print(minStreng.firstCapitalized())

// Output: Swift er gøy å lære!
```

## Dykk dypere

Det er verdt å merke seg at disse metodene vil bare fungere for engelsk tekst. Hvis du jobber med andre språk, må du bruke et mer avansert tilnærming for å sikre at alle bokstavene blir korrekt kapitalisert. Du kan også bruke `NSString`-klassen for å få tilgang til flere metoder for å endre store og små bokstaver.

## Se også

- [Apple's offisielle dokumentasjon om kapitalisering av strenger i Swift](https://developer.apple.com/documentation/swift/string/capitalizing_letters_in_a_string)
- [En samling av nyttige Swift-tips fra Ray Wenderlich](https://www.raywenderlich.com/100-swiftui-tips)