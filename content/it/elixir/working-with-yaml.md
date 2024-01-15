---
title:                "Lavorare con yaml"
html_title:           "Elixir: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché
Se sei un programmatore e stai lavorando con applicazioni che richiedono il processamento di dati strutturati, allora dovresti sicuramente considerare utilizzare YAML. Questo linguaggio di markup è flessibile, facile da leggere e supportato da diverse librerie di programmazione, rendendolo una scelta popolare per l'elaborazione di dati.

## Come
Per utilizzare YAML in Elixir, puoi utilizzare la libreria di terze parti "YamlElixir". Inizia installando la libreria con `mix add yamlelixir`. Quindi importa il modulo YamlElixir nel tuo progetto ed esegui il parsing di un file YAML utilizzando la funzione `YamlElixir.parse_file/1`. Di seguito un esempio:

```Elixir
import YamlElixir # Importa il modulo
file = "config.yml" # Definisce il percorso del file YAML
config = YamlElixir.parse_file(file) # Esegue il parsing del file
IO.inspect config # Stampa il contenuto del file
```

Output:
```
%{"database" => %{"host" => "localhost", "port" => 5432, "username" => "myuser", "password" => "secret"}, "environment" => "dev", "logging" => %{"level" => "info", "file" => "app.log"}, "api" => %{"base_url" => "https://example.com/api"}}
```

## Deep Dive
Oltre al parsing, YAML offre anche la possibilità di serializzare dati in un formato di file facilmente leggibile da esseri umani. Puoi utilizzare la funzione `YamlElixir.dump/2` per convertire un elenco di chiavi e valori in un formato YAML. Di seguito un esempio:

```Elixir
data = %{"name" => "John", "age" => 30, "hobbies" => ["programming", "hiking", "photography"]}
YamlElixir.dump(data, "output.yml") # Serializza i dati nel file "output.yml"
```

Output (contenuto del file "output.yml"):
```
name: John
age: 30
hobbies: 
- programming
- hiking
- photography
```

Inoltre, puoi utilizzare opzioni come `indent` per specificare il numero di spazi da utilizzare per indentare il file YAML e `canonical` per ordinare i dati in ordine alfabetico prima di serializzarli.

## Vedi anche
- [Documentazione della libreria YamlElixir](https://hexdocs.pm/yamlelixir)
- [Sintassi YAML](https://yaml.org/spec/1.2/spec.html)