---
title:                "Lavorare con JSON"
date:                  2024-01-19
html_title:           "Arduino: Lavorare con JSON"
simple_title:         "Lavorare con JSON"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/working-with-json.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con JSON significa manipolare dati in un formato leggero di interscambio. I programmatori lo fanno per la sua semplicità e ampia adozione nelle API web e nelle configurazioni.

## Come fare:
```Go
package main

import (
	"encoding/json"
	"fmt"
	"log"
)

// Struttura di esempio
type Utente struct {
	Nome      string `json:"nome"`
	Età       int    `json:"età"`
	HaAccesso bool   `json:"ha_accesso"`
}

func main() {
	// Serializzazione: Go -> JSON
	utente := &Utente{
		Nome:      "Giovanni",
		Età:       30,
		HaAccesso: true,
	}
	utenteJSON, err := json.Marshal(utente)
	if err != nil {
		log.Fatalf("Errore durante la serializzazione: %v", err)
	}
	fmt.Printf("%s\n", utenteJSON)

	// Deserializzazione: JSON -> Go
	datiJSON := `{"nome":"Francesca","età":25,"ha_accesso":false}`
	var utenteNuovo Utente
	if err := json.Unmarshal([]byte(datiJSON), &utenteNuovo); err != nil {
		log.Fatalf("Errore durante la deserializzazione: %v", err)
	}
	fmt.Printf("%+v\n", utenteNuovo)
}
```
Output:
```
{"nome":"Giovanni","età":30,"ha_accesso":true}
{Nome:Francesca Età:25 HaAccesso:false}
```

## Approfondimento
JSON, o JavaScript Object Notation, si è evoluto dalla notazione letterale degli oggetti in JavaScript. Alternative a JSON includono XML e YAML. La libreria standard di Go, `encoding/json`, implementa la codifica e decodifica efficiente di JSON e lavora bene con i tipi nativi di Go grazie all'uso di riflessione e tag di struct.

## Vedi Anche
- La documentazione ufficiale di Go su JSON: https://golang.org/pkg/encoding/json/
- Articolo introduttivo su JSON: https://www.json.org/json-it.html
- Best practice e consigli per lavorare con JSON in Go: https://blog.golang.org/json
