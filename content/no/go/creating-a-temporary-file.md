---
title:                "Oppretting av midlertidig fil"
html_title:           "Go: Oppretting av midlertidig fil"
simple_title:         "Oppretting av midlertidig fil"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Hva & hvorfor?
En midlertidig fil er en fil som opprettes midlertidig av en datamaskinprosess og slettes automatisk når prosessen avsluttes. Programmere bruker midlertidige filer for å lagre midlertidig data eller utføre operasjoner som krever en fil som ikke skal lagres permanent.

Hvordan:
Go har en innebygd funksjon, "ioutil.TempFile", som lar deg opprette midlertidige filer. Her er en kodeeksempel som viser hvordan du kan bruke denne funksjonen:

```Go
f, err := ioutil.TempFile("", "mytempfile")
if err != nil {
    panic(err)
}
defer os.Remove(f.Name())
```

Denne koden vil opprette en midlertidig fil i det gjeldende arbeidsområdet med prefikset "mytempfile" i filnavnet. Ved å bruke "defer" vil filen automatisk bli slettet når funksjonen er ferdig.

Dypdykk:
Opprettelse av midlertidige filer har vært en vanlig praksis i mange programmeringsspråk lenge før det ble innlemmet i Go. Et alternativ til å bruke "ioutil.TempFile" i Go, er å bruke "os.CreateTemp" som også tilbyr mer kontroll over stien og navn på den midlertidige filen.

See Also:
Du kan lære mer om opprettelse og bruk av midlertidige filer i Go ved å lese dokumentasjonen iGoDocs(https://golang.org/pkg/io/ioutil/#TempFile) og iGoRepo's kildekode(https://github.com/golang/go/tree/master/src/io).

Takk og god kodet! 🚀