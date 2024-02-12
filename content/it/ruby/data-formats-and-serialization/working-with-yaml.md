---
title:                "Lavorare con YAML"
aliases:
- /it/ruby/working-with-yaml/
date:                  2024-02-03T19:26:19.122221-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
YAML, che sta per YAML Ain't Markup Language, è ampiamente utilizzato in Ruby per file di configurazione e serializzazione dei dati grazie al suo formato facilmente leggibile. I programmatori si orientano verso YAML quando hanno bisogno di memorizzare o trasmettere oggetti dati in modo leggibile ma strutturato, semplificando compiti come la gestione della configurazione, lo stoccaggio dei dati e la condivisione di dati tra linguaggi differenti.

## Come fare:
Ruby include una libreria integrata chiamata Psych per l'analisi e l'emissione di YAML. Per utilizzarla, devi prima richiedere la libreria standard YAML. Ecco un esempio base per iniziare:

```ruby
require 'yaml'

# Hash da serializzare
person = { name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"] }

# Conversione dell'hash in YAML
yaml_data = person.to_yaml

puts yaml_data
```

**Output dell'esempio:**

```yaml
---
:name: John Doe
:age: 30
:skills:
- Ruby
- JavaScript
```

Per caricare dati YAML di nuovo in un oggetto Ruby:

```ruby
loaded_person = YAML.load(yaml_data)

puts loaded_person
```

**Output dell'esempio:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

### Utilizzo di librerie di terze parti:

Sebbene la libreria standard sia sufficiente per compiti basilari, per esigenze complesse potresti considerare l'uso di gemme di terze parti come 'safe_yaml'. Per utilizzare tali librerie, devi prima installare la gemma:

```bash
gem install safe_yaml
```

Successivamente, puoi usarla per caricare in modo sicuro i dati YAML, mitigando rischi come l'istanza di oggetti da fonti controllate dall'utente:

```ruby
require 'safe_yaml'

safe_loaded_person = SafeYAML.load(yaml_data)

puts safe_loaded_person
```

**Output dell'esempio:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

Questo approccio aumenta la sicurezza della tua gestione di YAML, rendendolo una buona scelta per applicazioni che caricano YAML da fonti non affidabili.
