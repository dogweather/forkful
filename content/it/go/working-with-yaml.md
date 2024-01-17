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

## Cosa & Perché?
Il lavoro con YAML è un modo per organizzare dati o configurazioni in modo leggibile e strutturato. I programmatori lo usano per semplificare la loro gestione dei dati e facilitare l'interscambio di informazioni tra le applicazioni.

## Come fare:
Ecco un esempio di come leggere e scrivere file YAML in Go:
```Go
// Import biblioteca YAML
import "gopkg.in/yaml.v2"

// Definizione di una struttura dati
type Person struct {
    Name string `yaml:"nome"`
    Age int `yaml:"eta"`
    Hobbies []string `yaml:"hobbies"`
}

func main() {
    // Lettura da un file YAML
    yamlFile, err := ioutil.ReadFile("persona.yml")
    if err != nil {
        panic(err)
    }

    // Parsing del file in una struttura dati
    var p Person
    err = yaml.Unmarshal(yamlFile, &p)
    if err != nil {
        panic(err)
    }

    // Stampa dei dati
    fmt.Println(p.Name)
    fmt.Println(p.Age)
    fmt.Println(p.Hobbies)

    // Scrittura in un file YAML
    newData := Person{
        Name: "Maria",
        Age: 28,
        Hobbies: []string{"musica", "lettura", "viaggi"},
    }
    yamlData, err := yaml.Marshal(newData)
    err = ioutil.WriteFile("nuova_persona.yml", yamlData, 0644)
    if err != nil {
        panic(err)
    }
}
```
Output:
```
John
35
[programming hiking cooking]
```

## Approfondimento:
### Contesto storico:
YAML è stato creato da Clark Evans nel 2001 come formato data serialization leggibile dall'uomo. Da allora, è diventato uno standard nella comunità dello sviluppo software per la sua semplicità e flessibilità.

### Alternative:
Ci sono diversi formati alternativi per la serializzazione dei dati, inclusi JSON, XML e CSV. YAML è preferito in quanto è più leggibile e facilmente modificabile dall'uomo rispetto ad altri formati.

### Dettagli di implementazione:
La libreria "gopkg.in/yaml.v2" offre funzionalità complete di analisi e formattazione dei dati YAML. È importante notare che la sintassi di YAML può essere sensibile agli spazi e agli indentazioni, quindi è necessario prestare attenzione alla struttura del file YAML per evitare errori di parsing.

## Vedi anche:
- Documentazione ufficiale di Go per YAML: https://pkg.go.dev/gopkg.in/yaml.v2
- Tutorial interattivo sulla manipolazione dei dati YAML in Go: https://gobyexample.com/yaml