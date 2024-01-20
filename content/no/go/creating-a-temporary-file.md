---
title:                "Opprette en midlertidig fil"
html_title:           "C#: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Opprette midlertidige filer i Go: En rask guide

## Hva & Hvorfor?
Å opprette en midlertidig fil er en tom fil laget for kortsiktig bruk ofte under en unik filnavn. Dette brukes av programmerere for å lagre data midlertidig uten å belaste minnet.

## Hvordan gjøre det:
Lag en midlertidig fil i Go ved å bruke `ioutil.TempFile` funksjonen. 

```Go
package main

import (
    "io/ioutil"
    "log"
    "os"
)

func main() {
    tempFile, err := ioutil.TempFile("", "sample")
    if err != nil {
        log.Fatal(err)
    }
    defer os.Remove(tempFile.Name())
    
    log.Println("Temp file created:", tempFile.Name())
}
```

Når du kjører dette scriptet, vil du se noe slik som:

```sh
2022/02/12 13:31:27 Temp file created: /tmp/sample865445024
```

## Dypdykk
Historisk sett har midlertidige filer blitt brukt i de fleste programmeringsspråk for å håndtere store datamengder uten å overbelaste internminnet. Go følger denne tradisjonen.

Det finnes flere alternativer for å håndtere midlertidig data lagring, som f.eks. memcached eller Redis, men midlertidige filer er enkel å bruke og trenger ingen eksterne avhengigheter.

`ioutil.TempFile` funksjonen i Go skaper faktisk en unik filnavn hver gang den blir kalt, noe som eliminerer kollisjonsproblemet vanlig i midlertidig filopprettelse. Dette skjer ved å tilføye et tilfeldig tall til filnavnet.

## Se også
For mer informasjon om dette emnet, se følgende lenker:
1. Offisiell Go dok: [ioutil.TempFile](https://pkg.go.dev/io/ioutil#TempFile)
2. Go blog: [File handling in Go](https://go.dev/blog/defer-panic-and-recover)
3. Tutorial: [How to Create Temp Files in Go](https://flaviocopes.com/go-tmp-files/)