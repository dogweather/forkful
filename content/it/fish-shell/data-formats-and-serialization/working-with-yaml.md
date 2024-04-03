---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:49.285943-07:00
description: "Lavorare con YAML implica analizzare e manipolare file YAML (YAML Ain't\
  \ Markup Language), un formato di serializzazione dei dati utilizzato per file di\u2026"
lastmod: '2024-03-13T22:44:43.880059-06:00'
model: gpt-4-0125-preview
summary: Lavorare con YAML implica analizzare e manipolare file YAML (YAML Ain't Markup
  Language), un formato di serializzazione dei dati utilizzato per file di configurazione,
  in Fish Shell.
title: Lavorare con YAML
weight: 41
---

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
