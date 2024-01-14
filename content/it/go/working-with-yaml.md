---
title:                "Go: Lavorare con YAML"
simple_title:         "Lavorare con YAML"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Go e stai cercando un modo per gestire i dati di configurazione in modo flessibile, allora lavorare con YAML potrebbe essere la soluzione perfetta per te. Con il suo formato leggibile per gli umani e la sua compatibilità con vari linguaggi di programmazione, YAML è diventato uno standard nella gestione dei dati di configurazione.

## Come fare

Per iniziare a lavorare con YAML in Go, è necessario prima importare il pacchetto "yaml" nel tuo progetto. Quindi, puoi utilizzare la funzione "marshal" per convertire una struttura di dati di Go in un documento YAML e la funzione "unmarshal" per convertire un documento YAML in una struttura di dati di Go.

Ecco un esempio di come convertire una struttura di dati di Go in un documento YAML:

```Go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
)

type Person struct {
  Name string `yaml:"name"`
  Age  int    `yaml:"age"`
}

func main() {
  p := Person{"Mario", 30}

  data, err := yaml.Marshal(p)
  if err != nil {
    panic(err)
  }
  fmt.Println(string(data))
}
```

L'output di questo codice sarebbe:

```
name: Mario
age: 30
```

E per convertire un documento YAML in una struttura di dati di Go:

```Go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
)

type Person struct {
  Name string `yaml:"name"`
  Age  int    `yaml:"age"`
}

func main() {
  data := `
    name: Luigi
    age: 35
  `
  p := Person{}

  err := yaml.Unmarshal([]byte(data), &p)
  if err != nil {
    panic(err)
  }
  fmt.Println(p)
}
```

L'output sarebbe:

```
{Luigi 35}
```

## Approfondimento

Oltre alle funzioni di base per la conversione dei dati, il pacchetto "yaml" offre anche vari tipi di strumenti per la manipolazione dei documenti YAML. Ad esempio, è possibile utilizzare la funzione "MapSlice" per creare facilmente un documento YAML da una mappa:

```Go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
)

func main() {
  m := yaml.MapSlice{
    {"frutta", "mela"},
    {"verdura", "spinaci"},
    {"carne", "pollo"},
  }

  data, err := yaml.Marshal(m)
  if err != nil {
    panic(err)
  }
  fmt.Println(string(data))
}
```

L'output sarebbe:

```
- frutta: mela
- verdura: spinaci
- carne: pollo
```

Per ulteriori informazioni sul pacchetto "yaml" e sui suoi strumenti, assicurati di leggere la documentazione ufficiale.

## Vedi anche

- [Documentazione del pacchetto "yaml" di Go](https://pkg.go.dev/gopkg.in/yaml.v3)
- [Introduzione a YAML](https://www.codeproject.com/Articles/1214409/YAML-For-Beginners)
- [Confronto tra YAML e JSON](https://medium.com/@alexolivier/yaml-vs-json-14031dba87f9)