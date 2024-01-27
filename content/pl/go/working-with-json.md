---
title:                "Praca z JSON"
date:                  2024-01-19
html_title:           "Bash: Praca z JSON"
simple_title:         "Praca z JSON"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Pracujemy z JSON, bo to prosta i powszechna forma wymiany danych. Używamy go w API, konfiguracjach i wszędzie tam, gdzie potrzebne jest przechowywanie czy przesyłanie informacji w czytelnym formacie.

## How to:
```Go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type Samochod struct {
    Marka string `json:"marka"`
    Model string `json:"model"`
    Rok   int    `json:"rok"`
}

func main() {
    // JSON do struktury
    jsonDane := `{"marka": "Fiat", "model": "500", "rok": 2012}`
    var samochod Samochod
    err := json.Unmarshal([]byte(jsonDane), &samochod)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("JSON do struktury: %+v\n", samochod)

    // Struktura do JSON
    nowySamochod := Samochod{"Skoda", "Octavia", 2020}
    toJson, err := json.Marshal(nowySamochod)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("Struktura do JSON: %s\n", toJson)
}
```
Wynik:
```
JSON do struktury: {Marka:Fiat Model:500 Rok:2012}
Struktura do JSON: {"marka":"Skoda","model":"Octavia","rok":2020}
```
## Deep Dive:
JSON, JavaScript Object Notation, wywodzi się z JavaScript, ale jest niezależny od języka. Chociaż XML był wcześniej popularny, JSON stał się dominujący ze względu na prostotę i szybkość.

Inne formaty jak YAML mogą być alternatywą dla JSON, ale nie są tak wszechstronne. W Go używamy pakietu `encoding/json` do pracy z JSON, który wewnętrznie bazuje na refleksji, by przypisać dane do struktur.

## See Also:
- [Oficjalna dokumentacja JSON w Go](https://golang.org/pkg/encoding/json/)
- [Tutorial: Jak używać JSON w Go](https://blog.golang.org/json)
- [JSON na Wikipedii](https://pl.wikipedia.org/wiki/JSON)
- [Porównanie JSON i XML](https://www.json.org/json-pl.html)
