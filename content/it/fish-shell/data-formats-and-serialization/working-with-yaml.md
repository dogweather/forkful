---
title:                "Lavorare con YAML"
aliases:
- /it/fish-shell/working-with-yaml/
date:                  2024-02-03T19:25:49.285943-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con YAML implica analizzare e manipolare file YAML (YAML Ain't Markup Language), un formato di serializzazione dei dati utilizzato per file di configurazione, in Fish Shell. I programmatori fanno ciò per automatizzare e configurare applicazioni o servizi in modo efficiente all'interno del contesto degli ambienti shell, facilitando compiti come la gestione delle configurazioni e il provisioning delle risorse.

## Come fare:
Fish Shell non ha un supporto integrato per l'analisi di YAML, ma è possibile utilizzare strumenti di terze parti come `yq` (un processore YAML da linea di comando leggero e portatile) per gestire i dati YAML.

**Installazione di yq (se non già installato):**
```fish
sudo apt-get install yq
```

**Leggere un valore da un file YAML:**
Supponiamo di avere un file YAML `config.yaml` con il seguente contenuto:
```yaml
database:
  host: localhost
  port: 3306
```

Per leggere l'host del database, si utilizza:
```fish
set host (yq e '.database.host' config.yaml)
echo $host
```
**Output di esempio:**
```
localhost
```

**Aggiornare un valore in un file YAML:**
Per aggiornare la `porta` a `5432`, usare:
```fish
yq e '.database.port = 5432' -i config.yaml
```
**Verificare l'aggiornamento:**
```fish
yq e '.database.port' config.yaml
```
**Output di esempio:**
```
5432
```

**Scrivere un nuovo file YAML:**
Per creare un nuovo `new_config.yaml` con contenuto predefinito:
```fish
echo "webserver:
  host: '127.0.0.1'
  port: 8080" | yq e -P - > new_config.yaml
```
Questo utilizza `yq` per elaborare e stampare in maniera ordinata (-P flag) una stringa in un nuovo file YAML.

**Analizzare strutture complesse:**
Se si dispone di un file YAML più complesso e si ha bisogno di estrarre array o oggetti annidati, si può:
```fish
echo "servers:
  - name: server1
    ip: 192.168.1.101
  - name: server2
    ip: 192.168.1.102" > servers.yaml

yq e '.servers[].name' servers.yaml
```
**Output di esempio:**
```
server1
server2
```
Utilizzando `yq`, Fish Shell rende semplice navigare attraverso documenti YAML e manipolarli per varie attività di automazione e configurazione.
