---
date: 2024-01-26 04:23:54.531560-07:00
description: "TOML sta per Tom's Obvious, Minimal Language (Linguaggio Minimo e Ovvio\
  \ di Tom). Viene utilizzato per i file di configurazione perch\xE9 \xE8 facile da\
  \ leggere\u2026"
lastmod: '2024-03-13T22:44:43.414685-06:00'
model: gpt-4-0125-preview
summary: "TOML sta per Tom's Obvious, Minimal Language (Linguaggio Minimo e Ovvio\
  \ di Tom). Viene utilizzato per i file di configurazione perch\xE9 \xE8 facile da\
  \ leggere\u2026"
title: Lavorare con TOML
weight: 39
---

## Cosa & Perché?
TOML sta per Tom's Obvious, Minimal Language (Linguaggio Minimo e Ovvio di Tom). Viene utilizzato per i file di configurazione perché è facile da leggere e scrivere per gli umani, pur essendo facile da analizzare per le macchine. Gli sviluppatori optano per TOML per evitare l'ingombro di XML e le complessità di JSON quando manipolano le configurazioni.

## Come fare:
Per gestire TOML in Kotlin, potresti usare una libreria come `ktoml`. Prima, aggiungiamo la dipendenza nel tuo `build.gradle.kts`:

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

Ora, analizziamo un po' di TOML:

```kotlin
import com.akuleshov7.ktoml.file.TomlFileReader

fun main() {
    val contenutoToml = TomlFileReader.readAndParseFile("config.toml")

    val configurazioneDatabase = contenutoToml.getTable("database")
    val host = configurazioneDatabase.getString("host")
    val porta = configurazioneDatabase.getLong("port")

    println("Host Database: $host")
    println("Porta Database: $porta")
}
```

Assumendo che `config.toml` sia così:

```toml
[database]
host = "localhost"
port = 5432
```

Un esempio di output sarebbe:

```
Host Database: localhost
Porta Database: 5432
```

## Approfondimento
TOML, ideato dal co-fondatore di GitHub Tom Preston-Werner nel 2013, mirava ad essere più diretto di YAML e più type-safe di JSON. È diventato un successo, soprattutto con il `Cargo` di Rust e il sistema dei moduli di Go. Alternative? YAML ha più funzionalità, JSON si traduce direttamente in oggetti in molti linguaggi di programmazione, e c'è sempre il vecchio e buon XML. Per quanto riguarda l'implementazione, ktoml, sotto licenza Apache 2.0, è una libreria puramente Kotlin e non si porta dietro librerie Java, offrendo anche DSL per scrivere TOML, non solo per leggerlo.

## Vedi Anche
- Il GitHub di TOML: https://github.com/toml-lang/toml
- Il GitHub di ktoml: https://github.com/akuleshov7/ktoml
- TOML vs. YAML vs. JSON: https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/
