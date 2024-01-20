---
title:                "Lavorare con yaml"
html_title:           "Bash: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

#Cos'è ed perché utilizzarlo?

Il formato YAML è un linguaggio di markup leggibile dall'uomo utilizzato per strutturare i dati in modo gerarchico e leggibile. I programmatori lo utilizzano soprattutto per l'organizzazione e la configurazione dei file di configurazione dei loro progetti.

#Come utilizzarlo:

```Bash
# Creazione di un file YAML
touch config.yaml

# Aggiunta di valori all'interno del file
echo "nome: Bash" >> config.yaml
echo "versione: 5.0" >> config.yaml
echo "linguaggio: scripting" >> config.yaml

# Lettura dei valori dal file
Bash config.yaml

# Output: nome: Bash
          versione: 5.0
          linguaggio: scripting
```

#Approfondimento:

##Contesto storico:

Il formato YAML è stato creato da Clark Evans nel 2001 come alternativa al formato XML per la strutturazione dei dati. Ha guadagnato popolarità tra i programmatori a partire dal 2006 ed è stato adottato come formato standard per la configurazione dei file di progetto da molte piattaforme di sviluppo.

##Alternative:

Alcune alternative al formato YAML includono JSON, XML e INI. Tuttavia, YAML è spesso preferito per la sua maggiore leggibilità e flessibilità nella strutturazione dei dati.

##Dettagli di implementazione:

Per utilizzare il formato YAML in Bash, è necessario installare il pacchetto python-yaml. È anche possibile utilizzare lo strumento di parsing bash YAML per leggere ed estrarre i dati da un file YAML.

#Vedi anche:

- [Documentazione ufficiale di YAML](https://yaml.org/spec/)
- [Strumento di parsing bash YAML](https://github.com/jasperes/bash-yaml)