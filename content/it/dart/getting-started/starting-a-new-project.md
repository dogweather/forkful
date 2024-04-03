---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:31.113201-07:00
description: "Avviare un nuovo progetto in Dart comporta l'impostazione di un ambiente\
  \ favorevole allo sviluppo efficiente, al testing e al deployment. I programmatori\u2026"
lastmod: '2024-03-13T22:44:43.131014-06:00'
model: gpt-4-0125-preview
summary: Avviare un nuovo progetto in Dart comporta l'impostazione di un ambiente
  favorevole allo sviluppo efficiente, al testing e al deployment.
title: Iniziare un nuovo progetto
weight: 1
---

## Cosa e Perché?

Avviare un nuovo progetto in Dart comporta l'impostazione di un ambiente favorevole allo sviluppo efficiente, al testing e al deployment. I programmatori avviano nuovi progetti Dart per sfruttare le ottimali prestazioni di Dart e il suo robusto ecosistema, in particolare per lo sviluppo di applicazioni web e mobile con framework come Flutter.

## Come fare:

1. **Installa Dart**:
   Verifica che Dart sia installato sul tuo sistema. In caso contrario, puoi scaricarlo da [https://dart.dev/get-dart](https://dart.dev/get-dart). Verifica l'installazione con:

   ```shell
   dart --version
   ```

2. **Crea un Nuovo Progetto Dart**:
   Utilizza la CLI di Dart per generare un nuovo progetto:

   ```shell
   dart create ciao_dart
   ```

   Questo comando crea una nuova directory `ciao_dart` con un semplice esempio di applicazione web o console, a seconda della tua scelta.

3. **Esamina la Struttura del Progetto**:
   
   Naviga nella directory del tuo progetto:

   ```shell
   cd ciao_dart
   ```

   Un tipico progetto Dart include i seguenti file e directory chiave:

   - `pubspec.yaml`: File di configurazione che include le dipendenze del tuo progetto e i vincoli SDK.
   - `lib/`: Directory dove risiede la maggior parte del codice Dart.
   - `test/`: Directory per i test del progetto.

4. **Aggiungi Dipendenze**:
   Modifica `pubspec.yaml` per aggiungere dipendenze. Per i progetti web, prendi in considerazione l'aggiunta di `http`, un pacchetto popolare per effettuare richieste HTTP:

   ```yaml
   dependencies:
     flutter:
       sdk: flutter
     http: ^0.13.3
   ```

   Dopo aver modificato, ottieni le dipendenze:

   ```shell
   dart pub get
   ```

5. **Scrivi il Tuo Primo Codice Dart**:
   
   Nella directory `lib/`, crea un nuovo file Dart, `main.dart`, e aggiungi un semplice codice Dart:

   ```dart
   // Importa la libreria core di Dart
   import 'dart:core';

   void main() {
     print('Ciao, Dart!');
   }
   ```

6. **Esegui la Tua Applicazione Dart**:

   Esegui il tuo programma Dart con:

   ```shell
   dart run
   ```

   L'output dovrebbe essere:

   ```
   Ciao, Dart!
   ```

Seguendo questi passaggi, hai avviato con successo un nuovo progetto Dart, dall'installazione all'esecuzione del tuo primo pezzo di codice Dart. Questa conoscenza di base apre la strada per immergersi più a fondo nell'ecosistema di Dart e nelle sue capacità di costruire applicazioni scalabili.
