---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:17.564914-07:00
description: "Nella programmazione, ottenere la data corrente \xE8 un compito fondamentale\
  \ che permette agli sviluppatori di accedere, visualizzare o manipolare la data\u2026"
lastmod: '2024-03-13T22:44:43.401186-06:00'
model: gpt-4-0125-preview
summary: "Nella programmazione, ottenere la data corrente \xE8 un compito fondamentale\
  \ che permette agli sviluppatori di accedere, visualizzare o manipolare la data\
  \ corrente all'interno delle loro applicazioni."
title: Ottenere la data corrente
weight: 29
---

## Cosa & Perché?
Nella programmazione, ottenere la data corrente è un compito fondamentale che permette agli sviluppatori di accedere, visualizzare o manipolare la data corrente all'interno delle loro applicazioni. Questa capacità è cruciale per tutto, dal registrare e marcare con timestamp gli eventi, fino alle calcolazioni basate sulle date.

## Come fare:

### Utilizzando Kotlin Standard
Kotlin non ha una propria API per data e ora, ma si affida alla Java Standard Library per questa funzionalità. Ecco come puoi ottenere la data corrente:

```kotlin
import java.time.LocalDate

fun main() {
    val oggi = LocalDate.now()
    println("Data di Oggi: $oggi")
}
```

**Esempio di output:**
```
Data di Oggi: 2023-04-05
```

### Utilizzando java.util.Date
Per operazioni che richiedono sia la data che l'ora, potresti preferire `java.util.Date`.

```kotlin
import java.util.Date

fun main() {
    val dataCorrente = Date()
    println("Data e Ora Correnti: $dataCorrente")
}
```

**Esempio di output:**
```
Data e Ora Correnti: Mer Apr 05 15:20:45 GMT 2023
```

### Utilizzando la Libreria Joda-Time
Prima che Java 8 introducesse una nuova API per Data e Ora, Joda-Time era lo standard de facto per le operazioni di data e ora in Java e Kotlin. Anche se ora non è più necessario per molti progetti, alcuni possono ancora utilizzarlo per motivi di legacy o preferenza personale.

Aggiungi la libreria Joda-Time al file build.gradle del tuo progetto:
```
implementation 'joda-time:joda-time:2.10.10'
```

```kotlin
import org.joda.time.LocalDate

fun main() {
    val oggi = LocalDate.now()
    println("Data di Oggi: $oggi")
}
```

**Esempio di output:**
```
Data di Oggi: 2023-04-05
```

### Utilizzando ThreeTenABP per Android
Per lo sviluppo Android, si raccomanda di utilizzare il backport dell'API Java Time tramite il ThreeTen Android Backport Project per le versioni precedenti al Livello API Android 26.

Aggiungi la dipendenza al file build.gradle della tua app:
```
implementation 'com.jakewharton.threetenabp:threetenabp:1.3.1'
```

Inizializzalo nella tua classe Application:
```kotlin
import android.app.Application
import com.jakewharton.threetenabp.AndroidThreeTen

class MyApp : Application() {
    override fun onCreate() {
        super.onCreate()
        AndroidThreeTen.init(this)
    }
}
```

Poi, puoi usarlo così:
```kotlin
import org.threeten.bp.LocalDate

fun main() {
    val oggi = LocalDate.now()
    println("Data di Oggi: $oggi")
}
```

**Esempio di output:**
```
Data di Oggi: 2023-04-05
```
