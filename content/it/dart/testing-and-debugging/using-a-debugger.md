---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:39.698164-07:00
description: "Utilizzare un debugger in Dart permette ai programmatori di esaminare\
  \ metodicamente il loro codice impostando breakpoint, eseguendo passo dopo passo\u2026"
lastmod: '2024-03-13T22:44:43.135754-06:00'
model: gpt-4-0125-preview
summary: "Utilizzare un debugger in Dart permette ai programmatori di esaminare metodicamente\
  \ il loro codice impostando breakpoint, eseguendo passo dopo passo\u2026"
title: Utilizzo di un debugger
---

{{< edit_this_page >}}

## Cosa & Perché?

Utilizzare un debugger in Dart permette ai programmatori di esaminare metodicamente il loro codice impostando breakpoint, eseguendo passo dopo passo l'esecuzione e ispezionando le variabili. Questo processo è essenziale per identificare e correggere gli errori in modo efficiente, rendendolo uno strumento indispensabile nel ciclo di sviluppo.

## Come fare:

### Debugging di Base:

**1. Impostare i Breakpoint:**

Per impostare un breakpoint, è sufficiente fare clic sul margine sinistro della riga di codice nel proprio IDE (ad es., Visual Studio Code o Android Studio) dove si desidera che l'esecuzione si interrompa.

```dart
void main() {
  var message = 'Ciao, Debugging';
  print(message); // Imposta un breakpoint qui
}
```

**2. Iniziare il Debugging:**

Nel proprio IDE, avviare una sessione di debugging facendo clic sull'icona di debug o premendo il pulsante di debug. L'esecuzione si interromperà ai breakpoint.

**3. Ispezionare le Variabili:**

Una volta interrotta l'esecuzione, passare con il mouse sulle variabili per vedere i loro valori attuali.

**4. Passare Attraverso il Codice:**

Utilizzare i comandi di passaggio oltre, entrata e uscita nel proprio IDE per navigare attraverso il codice una riga o funzione alla volta.

### Debugging Avanzato con Observatory:

Dart include uno strumento chiamato Observatory per il debugging e il profiling delle applicazioni Dart. È particolarmente utile per le applicazioni in esecuzione sulla VM Dart.

**Accesso a Observatory:**

Eseguire l'applicazione Dart con il flag `--observe`.

```bash
dart --observe your_program.dart
```

Questo comando stampa un URL nella console, che è possibile aprire in un browser web per accedere al debugger di Observatory.

### Utilizzo di Librerie di Terze Parti Popolari:

Per il debugging delle applicazioni Flutter, il pacchetto `flutter_devtools` offre un insieme di strumenti di performance e di debugging che si integrano sia con la VM Dart che con Flutter.

**Installazione:**

Prima, aggiungere `devtools` al file `pubspec.yaml` sotto `dev_dependencies`:

```yaml
dev_dependencies:
  devtools: any
```

**Lancio di DevTools:**

Eseguire questo comando nel proprio terminale:

```bash
flutter pub global run devtools
```

Quindi, avviare l'applicazione Flutter in modalità debug. DevTools offre funzionalità come l'ispettore Flutter per l'analisi dell'albero dei widget e il profiler di rete per monitorare l'attività di rete.

### Output di Esempio:

Una volta raggiunto un breakpoint, l'IDE potrebbe visualizzare i valori delle variabili e i tracciati dello stack in questo modo:

```
message: 'Ciao, Debugging'
```

Utilizzando efficacemente gli strumenti e le tecniche di debugging in Dart, gli sviluppatori possono identificare e risolvere i problemi più rapidamente, portando a un processo di sviluppo più fluido e a applicazioni più robuste.
