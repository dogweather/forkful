---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:43.068011-07:00
description: "YAML, acronimo di \"YAML Ain't Markup Language\", \xE8 uno standard\
  \ di serializzazione dei dati leggibile dall'uomo che pu\xF2 essere utilizzato per\
  \ i file di\u2026"
lastmod: '2024-03-13T22:44:43.619866-06:00'
model: gpt-4-0125-preview
summary: "YAML, acronimo di \"YAML Ain't Markup Language\", \xE8 uno standard di serializzazione\
  \ dei dati leggibile dall'uomo che pu\xF2 essere utilizzato per i file di configurazione,\
  \ cos\xEC come nelle applicazioni in cui i dati vengono memorizzati o trasmessi."
title: Lavorare con YAML
weight: 41
---

## Cosa & Perché?

YAML, acronimo di "YAML Ain't Markup Language", è uno standard di serializzazione dei dati leggibile dall'uomo che può essere utilizzato per i file di configurazione, così come nelle applicazioni in cui i dati vengono memorizzati o trasmessi. I programmatori si orientano verso YAML per la sua chiarezza e semplicità, specialmente in progetti che coinvolgono configurazioni complesse o la necessità di strutture di dati facilmente modificabili.

## Come:

Lavorare direttamente con YAML in Bash richiede un po' di ingegnosità poiché Bash non ha supporto integrato per l'analisi di YAML. Tuttavia, puoi utilizzare strumenti esterni come `yq` (un processore di YAML da riga di comando leggero e portatile) per interagire efficientemente con i file YAML. Vediamo alcune operazioni comuni:

### Installare `yq`:

Prima di immergerti negli esempi, assicurati di avere `yq` installato. Puoi di solito installarlo dal tuo gestore di pacchetti, per esempio, su Ubuntu:

```bash
sudo apt-get install yq
```

Oppure puoi scaricarlo direttamente dal suo repository GitHub.

### Leggere un valore:

Supponi di avere un file chiamato `config.yaml` con il seguente contenuto:

```yaml
database:
  host: localhost
  port: 5432
user:
  name: admin
  password: segreto
```

Per leggere l'host del database, puoi usare `yq` come segue:

```bash
yq e '.database.host' config.yaml
```

**Output Esempio:**

```
localhost
```

### Aggiornare un valore:

Per aggiornare il nome dell'utente in `config.yaml`, usa il comando `yq eval` con l'opzione `-i` (sul posto):

```bash
yq e '.user.name = "newadmin"' -i config.yaml
```

Verifica il cambiamento con:

```bash
yq e '.user.name' config.yaml
```

**Output Esempio:**

```
newadmin
```

### Aggiungere un nuovo elemento:

Per aggiungere un nuovo elemento nella sezione del database, come un nuovo campo `timeout`:

```bash
yq e '.database.timeout = 30' -i config.yaml
```

Controllare il contenuto del file confermerà l'aggiunta.

### Eliminare un elemento:

Per rimuovere la password sotto utente:

```bash
yq e 'del(.user.password)' -i config.yaml
```

Questa operazione rimuoverà il campo della password dalla configurazione.

Ricorda, `yq` è uno strumento potente e ha molte più capacità, inclusa la conversione di YAML in JSON, l'unione di file e manipolazioni ancora più complesse. Fai riferimento alla documentazione di `yq` per ulteriori esplorazioni.
