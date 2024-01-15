---
title:                "Lavorare con yaml"
html_title:           "Ruby: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

# Perché
## Perché dovresti lavorare con YAML

Se stai scrivendo un'applicazione Ruby che necessita di gestire dati strutturati in formato leggibile sia da computer che da esseri umani, YAML potrebbe essere lo strumento giusto per te. Con YAML puoi creare facilmente file di configurazione, documentazione e altri dati strutturati per la tua applicazione in modo semplice e intuitivo.

# Come fare
## Esempi di codice e output
Con la gemma ruby YAML incorporata, è molto facile iniziare a lavorare con questo formato di dati. Basta richiamare il modulo YAML e utilizzare il metodo `load` o `safe_load` per leggere i dati da un file YAML.

```Ruby
require 'yaml'

# Carica i dati da un file YAML
data = YAML.load(File.read("dati.yaml"))

# Oppure, utilizza safe_load per evitare stringhe malevole
data = YAML.safe_load(File.read("dati.yaml"))

puts data["nome"]
puts data["cognome"]
```

In questo esempio, abbiamo un file YAML `dati.yaml` con il seguente contenuto:

```yaml
nome: Marco
cognome: Rossi
```

E nell'output del codice sopra, otteniamo:

```shell
Marco
Rossi
```

Oltre a leggere i dati da un file, YAML ci permette anche di creare un data structure direttamente nel nostro codice.

```Ruby
require 'yaml'

# Crea un data structure
dati = {
  nome: "Maria",
  cognome: "Bianchi"
}

# Utilizza il metodo dump per convertire il data structure in YAML
puts YAML.dump(dati)
```

Questo ci darà un output come questo:

```yaml
---
:nome: Maria
:cognome: Bianchi
```

# Approfondimento
## Approfondisci l'utilizzo di YAML in Ruby

Se vuoi saperne di più su come utilizzare YAML nel tuo codice Ruby, puoi consultare la documentazione ufficiale di YAML e della gemma ruby incorporata. Inoltre, puoi anche esplorare altre gemme che offrono funzionalità aggiuntive per il lavoro con YAML, come ad esempio la gemma `psych`.

# Vedi anche
- Documentazione ufficiale di YAML: https://yaml.org/spec/
- Documentazione della gemma ruby YAML: https://ruby-doc.org/stdlib-2.7.1/libdoc/yaml/rdoc/YAML.html
- Gemma ruby psych: https://rubygems.org/gems/psych/