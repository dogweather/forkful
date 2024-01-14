---
title:    "Go: Uthenting av substringer"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/go/extracting-substrings.md"
---

{{< edit_this_page >}}

# Hvorfor

Å skrive kode som fungerer som det skal, kan være en kompleks oppgave, spesielt når det kommer til å behandle tekststrenger. Noen ganger ønsker vi kanskje å hente ut et bestemt utvalg av ord eller tegn fra en tekststreng. Dette kalles å ekstrahere substringer, og det kan være nyttig i mange situasjoner.

# Slik gjør du det

For å ekstrahere substringer i Go, kan vi bruke funksjonen `Substring()` fra standardbiblioteket `strings`. La oss si at vi har en tekststreng som heter `tekst` og vi ønsker å hente ut de første fem bokstavene. Da kan vi bruke følgende kode:

```Go
tekst := "Dette er en tekststreng"
substr := tekst[:5]
fmt.Println(substr)
```

Dette vil gi følgende output:

```
Dette
```

Her har vi brukt `[:5]` for å hente ut tegnene fra indeks 0 til 5 i tekststrengen. Vi kan også bruke `startIndex:endIndex` for å hente ut et bestemt antall tegn fra en tekststreng. La oss se på et annet eksempel:

```Go
tekst := "Dette er en tekststreng"
substr := tekst[8:12]
fmt.Println(substr)
```

Dette vil gi følgende output:

```
en t
```

Vi kan også bruke `Substring()` funksjonen til å hente ut en del av en tekststreng basert på et søkeord. For eksempel, hvis vi ønsker å hente ut ordet "tekst" fra tekststrengen vår, kan vi bruke følgende kode:

```Go
tekst := "Dette er en tekststreng"
searchWord := "tekst"
startIndex := strings.Index(tekst, searchWord)
substr := tekst[startIndex:startIndex+len(searchWord)]
fmt.Println(substr)
```

Dette vil gi følgende output:

```
tekst
```

# Få en dypere forståelse

Når vi bruker `Substring()` funksjonen, kan vi også angi en tredje parameter som representerer antall tegn vi ønsker å hente ut. For eksempel, hvis vi ønsker å hente ut de første tre ordene fra en tekststreng, kan vi bruke følgende kode:

```Go
tekst := "Dette er en tekststreng"
words := strings.Fields(tekst)
substr := strings.Join(words[:3], " ")
fmt.Println(substr)
```

Dette vil gi følgende output:

```
Dette er en
```

Vi kan også bruke `Substring()` funksjonen til å hente ut en del av en tekststreng basert på et bestemt mønster eller regulært uttrykk. Dette krever litt mer avansert kunnskap om regulære uttrykk, men det er nyttig å ha en forståelse av det når du jobber med tekstbehandling i Go.

# Se også

- [Dokumentasjon om `Substring()` funksjonen](https://golang.org/pkg/strings/#Substring)
- [Flere eksempler på å ekstrahere substringer i Go](https://gobyexample.com/slicing)
- [Lær mer om regulære uttrykk i Go](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-go)