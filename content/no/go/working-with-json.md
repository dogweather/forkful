---
title:                "Arbeide med json"
html_title:           "Go: Arbeide med json"
simple_title:         "Arbeide med json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Å jobbe med JSON er nyttig for å håndtere datautveksling på en enkel og effektiv måte. Det er et populært format som brukes for å overføre og lagre data, og derfor er det viktig å lære hvordan man kan jobbe med det i programmering.

## Hvordan

Koding med JSON er enkelt i Go, takket være innebygde pakker som "encoding/json". For å starte, må du deklarere en struktur som matcher JSON-dataene du vil jobbe med, og deretter kan du bruke "json.Unmarshal()" for å konvertere JSON-dataene til Go-strukturen. Her er et eksempel:

```
// Deklarerer en struktur som matcher JSON-data
type Person struct {
    Navn    string `json:"navn"`
    Alder   int    `json:"alder"`
    Hjemland string `json:"hjemland"`
}

// Deklarerer JSON-dataen som skal konverteres
jsonData := `{"navn": "Lise", "alder": 25, "hjemland": "Norge"}`

// Bruker json.Unmarshal() for å konvertere dataen
var person Person
err := json.Unmarshal([]byte(jsonData), &person)
if err != nil {
    fmt.Println(err)
}

// Printer ut dataen i Go-strukturen
fmt.Println(person.Navn) // output: Lise
fmt.Println(person.Alder) // output: 25
fmt.Println(person.Hjemland) // output: Norge 
```

## Dypdykk

Det er også mulig å jobbe med mer komplekse JSON-data, som inneholder nestede strukturer eller arrays. Du kan bruke for-løkker og "json.Unmarshal()" på nytt for å hente ut disse dataene. Du kan også bruke "json.Marshal()" for å konvertere Go-strukturer til JSON-format. Her er et eksempel på hvordan du kan håndtere nestede strukturer:

```
// Deklarerer en struktur som matcher JSON-data
type Film struct {
    Tittel   string `json:"tittel"`
    Aar      int    `json:"aar"`
    Sjangre  []string `json:"sjangre"`
}

// Deklarerer en struktur som inneholder en liste med filmer
type Filmer struct {
    Total int `json:"total"`
    Results []Film `json:"results"`
}

// Deklarerer JSON-data som skal konverteres
jsonData := `{
    "total": 3,
    "results": [
        {
            "tittel": "Ringenes Herre",
            "aar": 2001,
            "sjangre": ["Action", "Eventyr", "Fantasy"]
        },
        {
            "tittel": "Star Wars",
            "aar": 1977,
            "sjangre": ["Action", "Eventyr", "Sci-Fi"]
        },
        {
            "tittel": "Harry Potter",
            "aar": 2001,
            "sjangre": ["Fantasy"]
        }
    ]
}`

// Bruker json.Unmarshal() til å konvertere dataen
var filmer Filmer
err := json.Unmarshal([]byte(jsonData), &filmer)
if err != nil {
    fmt.Println(err)
}

// Printer ut dataen i Go-strukturen
for _, film := range filmer.Results {
    fmt.Println(film.Tittel) // output: Ringenes Herre
    fmt.Println(film.Aar) // output: 2001
    fmt.Println(film.Sjangre) // output: [Action Eventyr Fantasy]
}
```

## Se også

[Go sin offisielle tutorial om JSON](https://golang.org/pkg/encoding/json/)

[En oversikt over populære JSON-pakker for Go](https://awesome-go.com/#json)