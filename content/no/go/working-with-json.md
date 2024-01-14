---
title:                "Go: Å arbeide med json"
simple_title:         "Å arbeide med json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

I dagens digitale verden er JSON (JavaScript Object Notation) en av de mest brukte formatene for å utveksle data mellom systemer. Det gir en enkel og strukturert måte å lagre og overføre data på, noe som gjør det til et populært valg blant utviklere. I denne bloggposten vil vi se på hvorfor det er viktig å lære å jobbe med JSON i Go-programmering.

## Hvordan

Først må du importere Go sin built-in pakke for å jobbe med JSON:

```
import "encoding/json"
```

Deretter kan du deklarere og initialisere en struct som vil inneholde dataen som skal konverteres til JSON:

```
type Person struct {
    Name string `json:"name"`
    Age int `json:"age"`
    Hobby []string `json:"hobby"`
}
```

For å konvertere denne structen til JSON, kan du bruke funksjonen `Marshal` fra `json`-pakken:

```
p := Person{Name: "Johannes", Age: 27, Hobby: []string{"fotball", "musikk"}}
b, err := json.Marshal(p)
if err != nil {
    fmt.Println("Kunne ikke konvertere til JSON:", err)
}
fmt.Println(string(b))
```

Dette vil resultere i følgende utskrift:

```
{ "name": "Johannes", "age": 27, "hobby": ["fotball", "musikk"] }
```
For å konvertere JSON tilbake til en struct, kan du bruke funksjonen `Unmarshal`:

```
var p Person
err := json.Unmarshal(b, &p)
if err != nil {
    fmt.Println("Kunne ikke konvertere fra JSON:", err)
}
fmt.Println(p.Name) // Johannes
```

## Dypdykk

Når vi jobber med JSON i Go, er det viktig å forstå konseptet med tags i structen vår. Tags brukes til å definere hvordan feltene i structen skal konverteres til JSON-nøkler, som vi ser eksempler på i koden over. Dette gjør det enklere å definere og manipulere dataene våre når vi jobber med JSON.

Videre kan vi også bruke spesielle tags som `omitempty` og `string`, for å kontrollere hvordan dataene blir konvertert og om de skal inkluderes i JSON-konverteringen.

Når vi jobber med mer komplekse datastrukturer, kan vi også bruke unmarshaling med `interface{}`-objekter for å få tilgang til og manipulere dataene på en mer fleksibel måte.

## Se også

- [JSON og Go](https://blog.golang.org/json)
- [godoc for "encoding/json" pakken](https://golang.org/pkg/encoding/json/)
- [Offisiell dokumetasjon for JSON](https://www.json.org/json-en.html)