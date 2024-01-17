---
title:                "Arbeid med json"
html_title:           "Go: Arbeid med json"
simple_title:         "Arbeid med json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/working-with-json.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?
Å jobbe med JSON innebærer å kunne behandle data gjennom programmeringsspråket Go ved å konvertere informasjon til et enkelt og lettleselig format. Dette gjøres for å effektivt håndtere utveksling av data mellom forskjellige programmeringsspåk eller plattformer.

Hvordan:
Go tilbyr en enkel og effektiv måte å håndtere JSON på gjennom sitt innebygde "encoding/json" pakke. Ved å bruke funksjoner som "Marshal" og "Unmarshal" kan vi enkelt konvertere data til eller fra JSON-formatet. Se eksempler nedenfor for å få en bedre forståelse:

```Go
// Marshal data til JSON
data := map[string]string{
  "name": "John",
  "age": "30",
}
json, err := json.Marshal(data)
if err != nil {
  panic(err)
}
fmt.Println(string(json))

// Unmarshal JSON til data
jsonString := `{"name":"John","age":"30"}`
var data map[string]string
err := json.Unmarshal([]byte(jsonString), &data)
if err != nil {
  panic(err)
}
fmt.Println(data["name"])
```

Eksempel utoutput: {"name":"John","age":"30"}
John

Dypdykk:
JSON (JavaScript Object Notation) ble utviklet på 1990-tallet som en måte å kontekstfritt representere strukturerte data på. Siden da har det blitt en av de mest populære formater for datautveksling på nettet, spesielt i webapplikasjoner. Alternativer til å bruke Go for å jobbe med JSON inkluderer å bruke andre programmeringsspråk som allerede har integrert støtte for det, eller å bruke biblioteker som ikke følger Go's standarder. Å følge Go's standarder kan gjøre det enklere å vedlikeholde og utvide koden på lang sikt.

Se også:
- Offisiell Go dokumentasjon for JSON: https://golang.org/pkg/encoding/json/
- Eksempler på å bruke "encoding/json" pakken i Go: https://gobyexample.com/json
- Sammenligning mellom Go og andre programmeringsspråk for å jobbe med JSON: https://medium.com/always-be-coding/abc-go-programming-using-json-36a869eb13e2