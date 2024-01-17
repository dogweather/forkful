---
title:                "Utvinning av delstrenger"
html_title:           "Go: Utvinning av delstrenger"
simple_title:         "Utvinning av delstrenger"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva er det og hvorfor brukes det?
Ekstrahering av substrings er en metode som lar deg plukke ut en del av en tekststreng og lagre den som en egen variabel. Dette kan være nyttig når du bare trenger en del av en lengre tekst, for eksempel å isolere et søkeord fra en nettside eller å hente ut navnet på en fil fra en filsti. Programmere bruker dette for å gjøre koden sin mer effektiv og for å få nøyaktig den informasjonen de trenger.

## Slik gjør du det:
Hvis du vil ta ut en substring i Go, kan du bruke funksjonen `Substr()` sammen med indeksering. La oss si at du har en variabel `tekst` med en lang streng, og du bare vil ha de første fem bokstavene. Da kan du skrive `Substr(tekst, 0, 5)` for å få en ny variabel med kun de første fem tegnene. Her er et eksempel:

```Go
tekst := "Hei, mitt navn er Go programmering"
fmt.Println(Substr(tekst, 0, 5))
```
Dette vil gi følgende output:
```
Hei, m
```

## Dykk dypere:
Å extrahere substrings er ikke en ny metode og har blitt brukt i programmering i lang tid. Tidligere måtte man ofte bruke flere linjer med kode for å nå et tilsvarende resultat, men med Go sin enkle syntaks kan man nå gjøre det på en enkel linje. Alternativene til å bruke `Substr()` i Go er å bruke regulære uttrykk eller for-løkker, men disse metodene kan bli mer kompliserte og krever ofte mer kode. Ved å bruke `Substr()` gjør du koden din mer lesbar og effektiv.

## Se også:
- [Go sin offisielle dokumentasjon om Substr()](https://golang.org/pkg/strings/#Substring)
- [En grundig guide til å bruke Substr() i Go](https://www.calhoun.io/substringing/)
- [En annen kilde om alternativer til Substr() i Go](https://www.golangprograms.com/substring-a-string-in-golang.html)