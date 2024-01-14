---
title:                "Gleam: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché
Se ti occupi di programmazione, è molto probabile che tu abbia già sentito parlare di YAML. Questo linguaggio di markup è spesso utilizzato per definire le configurazioni dei programmi e dei servizi. Ma perché dovresti imparare ad usarlo? Semplicemente perché YAML è un modo semplice e intuitivo per rappresentare dati strutturati. Con esso, è possibile creare file di configurazione di alta qualità in modo rapido ed efficiente.

## Come Fare
Per utilizzare YAML in Gleam, basta importare il modulo `yaml` e siamo pronti per iniziare. Ecco un esempio di codice che definisce un semplice oggetto YAML e lo converte in un'intera stringa:

```Gleam
import yaml

my_config = {
  "site_name": "Il mio sito",
  "port": 8080,
  "database": {
    "host": "localhost",
    "username": "user",
    "password": "passw0rd"
  }
}

my_config_yaml = yaml.to_string(my_config)
```

Se vogliamo visualizzare l'output ottenuto, possiamo semplicemente scrivere `gleam run <nome_file>` dal terminale. Ecco cosa vedremo:

```yaml
site_name: Il mio sito
port: 8080
database:
  host: localhost
  username: user
  password: passw0rd
```

Come puoi notare, gli oggetti YAML sono rappresentati molto simile agli oggetti JSON, ma con una sintassi più pulita e leggibile.

## Deep Dive
Ora che siamo riusciti a creare un semplice file di configurazione YAML, vediamo alcune funzioni più avanzate che Gleam ci mette a disposizione per lavorare con questo linguaggio.

### Analisi e dereferenziazione
In alcuni casi, potremmo voler analizzare l'input di un file YAML e accedere a specifici valori. Per fare ciò, possiamo utilizzare l'operatore `->` seguito dalla chiave dell'oggetto che vogliamo dereferenziare. Ecco un esempio:

```Gleam
import yaml /_, build

#questo codice presuppone che il file "config.yaml" esista con lo stesso formato del precedente esempio

config = yaml.parse_file("config.yaml")

port = config -> "port"
database_host = config -> "database" -> "host"

// questi sarebbero rispettivamente 8080 e "localhost"
```

### Generazione di oggetti YAML
D'altro canto, se vogliamo creare un file YAML dinamicamente, possiamo farlo a partire da un oggetto Gleam che abbia una struttura precisa. Basta utilizzare la funzione `yaml.from_structure` ed ecco fatto! Ecco un esempio:

```Gleam
import yaml /_, build

person = {
  name: "Mario",
  age: 26,
  address: "Via del Corso, Roma"
}

person_yaml = yaml.from_structure(person)

// questo sarebbe il risultato
/* 
name: Mario
age: 26
address: Via del Corso, Roma 
*/
```

## Vedi Anche
- Documentazione di Gleam su YAML: https://gleam.run/modules/yaml
- Tutorial su YAML di Codecademy (in inglese): https://www.codecademy.com/courses/learn-yaml
- Esempi di parsing di file YAML in Gleam: https://github.com/gleam-lang/yaml/tree/master/examples