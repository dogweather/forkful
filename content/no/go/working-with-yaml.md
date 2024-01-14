---
title:                "Go: Jobbe med yaml"
simple_title:         "Jobbe med yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

YAML er et populært datatypingsformat for programmering, spesielt for konfigurasjonsfiler. Det er enkelt å lese og skrive, og kan brukes i en rekke ulike programmeringsspråk. Å lære å arbeide med YAML kan være svært nyttig for å gjøre programkodingen din mer effektiv og funksjonell.

## Hvordan

For å arbeide med YAML i Go, trenger du å importere "gopkg.in/yaml.v2" pakken. Her er et eksempel på hvordan du kan lese og skrive en YAML-fil i Go:

```Go
package main

// Importer den nødvendige pakken
import (
    "fmt"
    "log"

    "gopkg.in/yaml.v2"
)

// Definer en struct for YAML-dataen
type Person struct {
    Name string `yaml:"name"`
    Age  int    `yaml:"age"`
}

func main() {
    // Åpne YAML-filen
    file, err := ioutil.ReadFile("person.yaml")
    if err != nil {
        log.Fatalf("Feil ved åpning av YAML-fil: %v", err)
    }

    // Lag et tomt Person-objekt
    p := &Person{}

    // Unmarshal YAML-dataen inn i Person-objektet
    err = yaml.Unmarshal(file, p)
    if err != nil {
        log.Fatalf("Feil ved unmarshaling av YAML: %v", err)
    }

    // Skriv ut dataen
    fmt.Printf("Navn: %s\nAlder: %d", p.Name, p.Age)

    // Lag en ny YAML-fil med oppdaterte data
    p.Age = 30
    newFile, err := yaml.Marshal(p)
    if err != nil {
        log.Fatalf("Feil ved marshaling av YAML: %v", err)
    }

    // Skriv YAML-dataen til den nye filen
    err = ioutil.WriteFile("newperson.yaml", newFile, 0644)
    if err != nil {
        log.Fatalf("Feil ved skriving av YAML-fil: %v", err)
    }
}
```

Output:

```YAML
Name: John
Age: 30
```

## Deep Dive

YAML, som står for "YAML Ain't Markup Language", ble utviklet for å være et enkelt og leselig datatypingsformat. Det bruker en innrykk- og syntaksbasert struktur som er enkelt å forstå, selv for de som ikke er programmerere. Det støtter også kommentarer, noe som kan være nyttig for å gi forklaringer eller dokumentasjon i YAML-filene.

I Go, kan du bruke `yaml:",omitempty"` etter struct tagger for å unngå å skrive ut zero-value-felt. Dette kan være nyttig for å redusere størrelsen på YAML-filer og forbedre lesbarheten.

I tillegg kan du bruke `yaml:"-"` for å utelate et felt fra å bli unmarshalled eller marshalled. Dette kan være nyttig hvis du har et felt som ikke er relevant for YAML, men som du trenger å ha i structen din for andre formål.

Du kan også bruke innebygde strukturer i Go, som `map[string]interface{}`, for å unmarshalle YAML-dataen inn i mer komplekse strukturer.

For å lære mer om YAML i Go, kan du sjekke ut dokumentasjonen for "gopkg.in/yaml.v2" pakken og lese gjennom eksemplene på bruk.

## Se også

- [Dokumentasjon for gopkg.in/yaml.v2 pakken](https://pkg.go.dev/gopkg.in/yaml.v2)
- [YAML-spesifikasjonen](https://yaml.org/spec/)