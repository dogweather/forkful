---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:32.465143-07:00
description: "Come fare: Per iniziare a lavorare con TOML in Go, \xE8 necessario includere\
  \ una libreria che possa analizzare i file TOML poich\xE9 la libreria standard di\
  \ Go\u2026"
lastmod: '2024-03-13T22:44:42.933533-06:00'
model: gpt-4-0125-preview
summary: "Per iniziare a lavorare con TOML in Go, \xE8 necessario includere una libreria\
  \ che possa analizzare i file TOML poich\xE9 la libreria standard di Go non supporta\
  \ nativamente TOML."
title: Lavorare con TOML
weight: 39
---

## Come fare:
Per iniziare a lavorare con TOML in Go, è necessario includere una libreria che possa analizzare i file TOML poiché la libreria standard di Go non supporta nativamente TOML. Il pacchetto `BurntSushi/toml` è una scelta popolare per questo. Prima di tutto, assicurati di installarlo:

```bash
go get github.com/BurntSushi/toml
```

Ecco un semplice esempio di come utilizzarlo. Supponiamo che tu abbia un file di configurazione chiamato `config.toml` con il seguente contenuto:

```toml
title = "Esempio TOML"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Ora, devi creare una struttura Go che rifletta la struttura TOML:

```go
package main

import (
    "fmt"
    "github.com/BurntSushi/toml"
)

type Config struct {
    Title    string
    Database Database `toml:"database"`
}

type Database struct {
    Server        string
    Ports         []int
    ConnectionMax int `toml:"connection_max"`
    Enabled       bool
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Title: %s\n", config.Title)
    fmt.Printf("Database Server: %s\n", config.Database.Server)
}
```

Output dell'esempio:

```
Title: Esempio TOML
Database Server: 192.168.1.1
```

## Analisi approfondita
TOML è stato creato da Tom Preston-Werner, uno dei cofondatori di GitHub, per offrire un formato di file di configurazione diretto che può essere facilmente mappato su una tabella hash e compreso a colpo d'occhio senza conoscenza pregressa del formato. Si contrappone a JSON o YAML, che, sebbene anch'essi ampiamente utilizzati, possono essere meno amichevoli per l'uomo per i file di configurazione a causa di parentesi graffe, virgolette e problemi di indentazione.

Il pacchetto `BurntSushi/toml` in Go è una libreria robusta che permette non solo la decodifica ma anche la codifica dei file TOML, rendendolo una scelta versatile per le applicazioni che necessitano sia di leggere che di scrivere file di configurazione in questo formato. Tuttavia, è importante notare che con l'avanzamento delle tecnologie e l'introduzione di nuove versioni di Go, sono emerse alternative come `pelletier/go-toml`, che offrono prestazioni migliorate e funzionalità aggiuntive come la manipolazione e il supporto delle query sull'albero.

Sebbene TOML sia una scelta eccellente per molte applicazioni, a seconda della complessità della configurazione dell'applicazione e delle preferenze personali o del team, altri formati come YAML o JSON potrebbero essere più adatti, specialmente se la configurazione richiede strutture dati più complesse che la natura verbosa di TOML potrebbe non catturare elegantemente. Tuttavia, per configurazioni dirette, leggibili e facilmente modificabili, TOML, abbinato al solido sistema di tipizzazione di Go e alle biblioteche suddette, è un'ottima scelta.
