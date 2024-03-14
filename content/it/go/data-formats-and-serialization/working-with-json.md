---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:58.698524-07:00
description: "Lavorare con JSON (JavaScript Object Notation) in Go comporta la codifica\
  \ e decodifica dei dati tra le strutture dati di Go e il formato JSON. Questo\u2026"
lastmod: '2024-03-13T22:44:42.931352-06:00'
model: gpt-4-0125-preview
summary: "Lavorare con JSON (JavaScript Object Notation) in Go comporta la codifica\
  \ e decodifica dei dati tra le strutture dati di Go e il formato JSON. Questo\u2026"
title: Lavorare con JSON
---

{{< edit_this_page >}}

## Cosa & Perché?

Lavorare con JSON (JavaScript Object Notation) in Go comporta la codifica e decodifica dei dati tra le strutture dati di Go e il formato JSON. Questo compito è onnipresente nei servizi web e nelle API, poiché JSON funge da formato di interscambio di dati leggero, basato su testo e indipendente dal linguaggio, consentendo una semplice condivisione dei dati attraverso diversi ambienti di programmazione.

## Come fare:

In Go, il pacchetto `encoding/json` è il tuo accesso alla manipolazione di JSON, fornendo meccanismi per convertire le strutture dati di Go in JSON (marshalling) e viceversa (unmarshalling). Di seguito sono riportati esempi basilari per iniziare:

### Codifica (Marshalling)

Per convertire una struct di Go in JSON, puoi utilizzare `json.Marshal`. Considera la seguente struct di Go:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type User struct {
    ID        int      `json:"id"`
    Username  string   `json:"username"`
    Languages []string `json:"languages"`
}

func main() {
    user := User{1, "JohnDoe", []string{"Go", "JavaScript", "Python"}}
    userJSON, err := json.Marshal(user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(userJSON))
}
```

Output:

```json
{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}
```

### Decodifica (Unmarshalling)

Per analizzare JSON in una struttura dati di Go, usa `json.Unmarshal`:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

func main() {
    jsonStr := `{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}`
    var user User
    err := json.Unmarshal([]byte(jsonStr), &user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("%+v\n", user)
}
```

Dato lo struct `User` come prima, questo codice analizza la stringa JSON in un'istanza User.

Output:

```go
{ID:1 Username:JohnDoe Languages:[Go JavaScript Python]}
```

## Approfondimento

Il pacchetto `encoding/json` in Go offre un'API semplice che astrae gran parte della complessità coinvolta nella manipolazione di JSON. Introdotto all'inizio dello sviluppo di Go, questo pacchetto riflette la filosofia di Go di semplicità ed efficienza. Tuttavia, l'uso della riflessione da parte di `encoding/json` per ispezionare e modificare le struct in tempo di esecuzione può portare a prestazioni meno che ottimali in scenari intensivi sulla CPU.

Alternative come `json-iterator/go` e `ffjson` sono emerse, fornendo un'elaborazione JSON più rapida tramite la generazione di codice statico di marshalling e unmarshalling. Tuttavia, `encoding/json` rimane il pacchetto più comunemente utilizzato a causa della sua semplicità, robustezza e del fatto che fa parte della libreria standard, garantendo compatibilità e stabilità attraverso le versioni di Go.

Nonostante le sue prestazioni relative più lente, la facilità d'uso e l'integrazione con il sistema di tipi di Go rendono `encoding/json` adatto alla maggior parte delle applicazioni. Per coloro che lavorano in contesti in cui la performance è fondamentale, esplorare le librerie esterne può essere utile, ma per molti, la libreria standard offre il giusto equilibrio tra velocità, semplicità e affidabilità.
