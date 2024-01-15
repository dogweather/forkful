---
title:                "Lavorare con yaml"
html_title:           "Go: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché
Se sei un programmatore che lavora con YAML, allora la conoscenza di Go ti renderà più efficiente e produttivo. Con Go puoi facilmente creare, leggere e modificare i file YAML utilizzando moduli specifici.

## Come fare
Per iniziare a lavorare con YAML in Go, devi prima importare il pacchetto "gopkg.in/yaml.v2". Quindi, puoi utilizzare la funzione "Marshal" per convertire una struttura dati Go in un formato YAML. Ad esempio:

```Go
package main

import (
    "fmt"
    "gopkg.in/yaml.v2"
)

type Person struct {
    Name string
    Age  int
}

func main() {
    person := Person{Name: "Maria", Age: 30}
    yamlData, _ := yaml.Marshal(person)
    fmt.Println(string(yamlData))
}
```

Output:
```
name: Maria
age: 30
```

Per leggere un file YAML in una struttura dati Go, puoi utilizzare la funzione "Unmarshal". Ad esempio:

```Go
package main

import (
    "fmt"
    "io/ioutil"
    "gopkg.in/yaml.v2"
)

type Book struct {
    Title    string `yaml:"title"`
    Author   string `yaml:"author"`
    Language string `yaml:"language"`
}

func main() {
    yamlFile, _ := ioutil.ReadFile("book.yaml")
    book := Book{}
    err := yaml.Unmarshal(yamlFile, &book)
    if err != nil {
        panic(err)
    }
    fmt.Println(book.Title)
    fmt.Println(book.Author)
    fmt.Println(book.Language)
}
```

Book.yaml:
```
title: The Alchemist
author: Paulo Coelho
language: English
```

Output:
```
The Alchemist
Paulo Coelho
English
```

## Approfondimento
È importante notare che Go non ha una libreria YAML incorporata, ma fa affidamento sui moduli di terze parti. Uno dei vantaggi di utilizzare Go per lavorare con YAML è la velocità, poiché Go è un linguaggio compilato. Inoltre, il pacchetto "gopkg.in/yaml.v2" offre anche la possibilità di gestire dati YAML complessi, come strutture nidificate e variabili di tipo diverso.

## Vedi anche
- [Documentazione del pacchetto YAML di Go](https://godoc.org/gopkg.in/yaml.v2)
- [Tutorial su come lavorare con YAML in Go](https://www.sohamkamani.com/golang/parsing-json/)