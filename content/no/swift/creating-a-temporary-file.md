---
title:                "Oppretting av en midlertidig fil"
html_title:           "Swift: Oppretting av en midlertidig fil"
simple_title:         "Oppretting av en midlertidig fil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Oppretting av en midlertidig fil er en vanlig praksis blant programmerere for å midlertidig lagre data som er nødvendige for å utføre en bestemt operasjon. Dette kan være nyttig når man arbeider med store mengder data, eller når man bare trenger å lagre data midlertidig under utførelse av et program. 

## Hvordan:
```Swift 
let tempFile = try TemporaryFile()
tempFile.write(string: "Dette er en midlertidig fil")
tempFile.close()
```

Output: En midlertidig fil blir opprettet og teksten "Dette er en midlertidig fil" blir skrevet til filen før den blir lukket.

## Dykk dypere:
Opprettelsen av midlertidige filer har vært en vanlig praksis siden begynnelsen av databehandling. I gamle dager måtte man bruke kommandoer som "mktemp" for å opprette en midlertidig fil, men med dagens programmeringsspråk som Swift, er det mye enklere.

Det finnes også alternative måter å midlertidig lagre data på, som for eksempel å bruke minnet (RAM) eller å lagre data i en buffer. Men midlertidige filer er ofte den mest effektive metoden, spesielt når man arbeider med store mengder data.

Når man oppretter en midlertidig fil i Swift, vil filen automatisk bli slettet når programmet termineres eller når den siste referansen til filen blir frigitt. Dette gjør bruk av midlertidige filer både enkelt og trygt.

## Se også:
- Offisiell dokumentasjon for TemporaryFile i Swift: https://developer.apple.com/documentation/foundation/temporaryfile
- En artikkel om hvorfor man bør bruke midlertidige filer: https://medium.com/@fountainhead/dont-use-temporary-files-use-in-memory-storage-or-buffers-instead-b3e28134b959