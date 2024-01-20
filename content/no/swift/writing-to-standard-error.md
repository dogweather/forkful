---
title:                "Skrive til standardfeil"
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Standard error (stderr) er en stream for å skrive ut feilmeldinger. Vi bruker det for å skille vanlig output (stdout) fra feil og loggmeldinger, noe som hjelper i debugging og loggføring.

## How to:

```Swift
import Foundation

// Skrive en enkel feilmelding til stderr.
func writeToStdErr(message: String) {
    fputs("\(message)\n", stderr)
}

// Bruk den:
writeToStdErr(message: "En feilmelding har oppstått.")

// Eksempel på logging av feil med en datostempel
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
let currentDateTime = dateFormatter.string(from: Date())

let errorMsg = "Kritisk feil: Databasen er ikke tilgjengelig."
writeToStdErr(message: "\(currentDateTime) - \(errorMsg)")

```

Forventet output i konsollen:
```
En feilmelding har oppstått.
2023-03-23 18:45:12 - Kritisk feil: Databasen er ikke tilgjengelig.
```

## Deep Dive

Historisk sett, i Unix og Unix-lignende operativsystemer, er det tre hoved datastrømmer: standard input (stdin), standard output (stdout), og standard error (stderr). De var en del av original designen av Unix shell. Å skille stderr fra stdout lar oss omdirigere dem separat i shell, noe som kan være nyttig for automatisk prosessering av logger eller feil.

Som alternativ for `fputs`, kan du også bruke `FileHandle.standardError.write`. Mer avansert logging kan kreve bruk av loggerbiblioteker som `os.log` eller tredjepartsbiblioteker som gir mer funksjonalitet og konfigurasjonsmuligheter.

Implementeringsdetaljer involverer å forstå forskjellige streams og hvordan de håndteres av operativsystemet og Shell. Skrive til stderr er direkte i Swift og krever ingen ekstra biblioteker.

## See Also

- Swift's FileHandle Documentation: https://developer.apple.com/documentation/foundation/filehandle
- Apple's Unified Logging System: https://developer.apple.com/documentation/os/logging
- Redirecting Output in Shell: https://www.gnu.org/software/bash/manual/html_node/Redirections.html