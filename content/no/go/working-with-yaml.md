---
title:                "Arbeide med yaml"
html_title:           "Go: Arbeide med yaml"
simple_title:         "Arbeide med yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med YAML er å lage og lese strukturerte datafiler. Dette er nyttig for å organisere og dele informasjon i programmering. Ved å bruke YAML, kan programmene våre være mer effektive og skalerbare.

## Hvordan:
Kodeeksempler og output kan ses innenfor ```Go ... ``` kodeblokker.

For å lage en YAML-fil i Go, kan vi bruke pakken "gopkg.in/yaml.v2". Her er et eksempel på hvordan dette kan gjøres:
```Go
import (
    "fmt"
    "gopkg.in/yaml.v2"
)

type Person struct {
    Name  string `yaml:"name"`
    Age   int    `yaml:"age"`
    Email string `yaml:"email"`
}

func main() {
    // Oppretter en person med data
    p := Person{Name: "Lisa", Age: 25, Email: "lisa@example.com"}

    // Konverterer til YAML og skriver til konsoll
    yamlData, _ := yaml.Marshal(p)
    fmt.Printf(string(yamlData))
}
```
Dette vil gi følgende output:
```Go
name: Lisa
age: 25
email: lisa@example.com
```

For å lese en YAML-fil i Go, kan vi bruke samme pakke og metoden "yaml.Unmarshal()". Her er et eksempel på hvordan dette kan gjøres:
```Go
import (
    "fmt"
    "gopkg.in/yaml.v2"
)

type Person struct {
    Name  string `yaml:"name"`
    Age   int    `yaml:"age"`
    Email string `yaml:"email"`
}

func main() {
    // Definerer en variabel for å lagre YAML-data
    var data Person

    // Leser YAML-filen og lagrer i variabelen
    file, _ := ioutil.ReadFile("person.yaml")
    yaml.Unmarshal(file, &data)

    // Skriver dataen til konsoll
    fmt.Println(data.Name)
    fmt.Println(data.Age)
    fmt.Println(data.Email)
}
```
Hvis filen "person.yaml" inneholder følgende data:
```Go
name: Lisa
age: 25
email: lisa@example.com
```
Vil konsollen skrive ut:
```
Lisa
25
lisa@example.com
```

## Dypdykk:
YAML står for "YAML Ain't Markup Language" og er et format som ble utviklet i begynnelsen av 2000-tallet. Det er et alternativ til XML og JSON, og er spesielt populært innenfor programmeringsverdenen.

En annen måte å jobbe med YAML i Go er å bruke pakken "gopkg.in/yaml.v3", som ble lansert i 2018 og er en oppdatering av versjon 2. Denne gir mer funksjonalitet og bedre ytelse, men krever også en annen konverteringsprosess. 

Implementasjonen av YAML i Go er basert på YAML 1.2-spesifikasjonen. Dette betyr at alle funksjonene og reglene i denne versjonen blir støttet. 

## Se også:
- Offisiell dokumentasjon for pakken "gopkg.in/yaml.v2": https://pkg.go.dev/gopkg.in/yaml.v2
- Oppdateringen av pakken til versjon 3: https://blog.golang.org/yaml-v3.0.0-released