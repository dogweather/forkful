---
title:                "Lavorare con TOML"
date:                  2024-01-26T04:21:29.031538-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con TOML"

category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/working-with-toml.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
TOML è un formato di file di configurazione, facile da leggere e scrivere per gli umani, e facile da analizzare e generare per le macchine. I programmatori lavorano con TOML per file di configurazione chiari e gerarchici nei progetti dove la leggibilità è fondamentale.

## Come fare:
Per leggere e manipolare TOML in Fish, potresti usare uno strumento come `yj`, che può convertire TOML in JSON. Ecco come:

```fish
# Installa yj tramite Fisher
fisher install jorgebucaran/yj

# Converti TOML in JSON
echo 'title = "Esempio TOML"' | yj -tj

# Esempio di output
{"title":"Esempio TOML"}
```

Per scrivere in TOML, inverti il processo:

```fish
# Converti JSON in TOML
echo '{"title":"Esempio JSON"}' | yj -jt

# Esempio di output
title = "Esempio JSON"
```

Per lavori più impegnativi, considera uno strumento CLI dedicato a TOML come `toml-cli`.

```fish
# Installa toml-cli
pip install toml-cli

# Imposta un valore nel file TOML
toml set pyproject.toml tool.poetry.version "1.1.4"

# Ottieni un valore dal file TOML
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## Approfondimento
TOML (Tom's Obvious, Minimal Language), introdotto da Tom Preston-Werner nel 2013, è simile a INI ma con una specifica definita e una gerarchia dei dati. JSON e YAML sono le principali alternative, ma hanno i loro compromessi: JSON non è così amichevole per gli umani, mentre YAML è più complesso. Il design di TOML eccelle in scenari in cui i file di configurazione sono spesso mantenuti manualmente, bilanciando semplicità ed espressività. Quando si tratta di implementazione, ci sono parser TOML disponibili per la maggior parte dei linguaggi di programmazione, inclusi TomlBombadil per Fish che può inserirsi direttamente nei tuoi script.

## Vedi Anche
- Specifica ufficiale di TOML: https://toml.io
- `yj`, uno strumento per convertire tra TOML, JSON, YAML e XML: https://github.com/jorgebucaran/yj
- `toml-cli`, un'utilità da linea di comando per TOML: https://github.com/sdispater/toml-cli
