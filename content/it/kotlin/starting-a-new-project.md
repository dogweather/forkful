---
title:                "Iniziare un nuovo progetto"
html_title:           "Arduino: Iniziare un nuovo progetto"
simple_title:         "Iniziare un nuovo progetto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Inizia un nuovo progetto in Kotlin - Guida rapida

## Che cos'è & Perché?
Dare il via a un nuovo progetto significa creare il proprio ambiente di codificazione e impostare tutto ciò che è essenziale per iniziare a scrivere il codice. I programmatori lo fanno per organizzare e gestire meglio il lavoro, dalle piccole funzioni alle gigantesche applicazioni.

## Come fare:
Avviare un nuovo progetto in Kotlin è piuttosto semplice. Ecco un esempio di come creare un nuovo progetto utilizzando l'IDE di IntelliJ IDEA:
  
```
1. Avvia IntelliJ IDEA e scegli "Create New Project" dal menu iniziale.
2. Seleziona "Kotlin" nella lista a sinistra e poi "JVM | IDEA" nel pannello a destra.
3. Dà un nome al tuo progetto e scegli la posizione dove vuoi salvarlo.
4. Clicca su "Finish" e aspetta che IntelliJ IDEA generi il tuo nuovo progetto.
```

E tutta quà! Ora hai il tuo nuovo progetto Kotlin pronto per essere codificato.

Per creare un semplice file Kotlin all'interno del progetto:

```
- Click destro sulla cartella src --> New --> Kotlin Class/File
- Dà un nome al tuo file. Per esempio "Main"
- Scrivi il tuo codice Kotlin. Per esempio un semplice "Hello, World!":

  ```Kotlin
  fun main() {
     println("Hello, World!")
  }
  ```

- Salva ed esegui il tuo codice.
```

L'output sarà:

```
Hello, World!
```

Ed ecco come si ottiene un nuovo progetto Kotlin e il primo "Hello, World!" codice.

## Approfondimento:
Sebbene gli IDE come IntelliJ IDEA rendano facile iniziare con Kotlin, è importante capire che tutto ciò che fanno è automatizzare un processo che si può fare manualmente. Inizialmente, Kotlin è stato progettato per essere completamente interoperabile con Java, e quindi la preparazione di un ambiente di progetto Kotlin è simile a quella di un progetto Java.

Esistono molte alternative a IntelliJ IDEA per iniziare un nuovo progetto Kotlin, inclusi Eclipse, Android Studio e la riga di commando. Ognuna di queste opzioni ha i suoi pro e i suoi contro, quindi è importante scegliere quello giusto per le tue esigenze. Per un approccio più manual e approfondito alla creazione di un progetto Kotlin, potresti esaminare l'uso di Gradle o Maven.

## Leggi anche:
Per approfondire l'iniziazione dei nuovi progetti Kotlin, consulta i seguenti link:
- [Documentazione Kotlin ufficiale](https://kotlinlang.org/docs/home.html)
- [Come creare un progetto tramite la riga di comando](https://kotlinlang.org/docs/command-line.html) 
- [Creare un progetto Kotlin con Maven](https://kotlinlang.org/docs/maven.html)

Buona programmazione a tutti voi, programmatori Kotlin!