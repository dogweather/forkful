---
title:                "Avviare un nuovo progetto"
html_title:           "Clojure: Avviare un nuovo progetto"
simple_title:         "Avviare un nuovo progetto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

Cosa & Perché?

Avviare un nuovo progetto in Clojure significa creare un nuovo ambiente di sviluppo per scrivere codice in questo linguaggio di programmazione funzionale. I programmatori generalmente iniziano nuovi progetti quando devono affrontare un nuovo problema o quando vogliono aggiornare o sostituire un progetto esistente.

Come fare:

```Clojure
;; Per iniziare un nuovo progetto in Clojure, dobbiamo installare il framework di gestione dei pacchetti Leiningen. Dopo l'installazione, possiamo creare un nuovo progetto usando il comando "lein new". Ad esempio, per creare un nuovo progetto chiamato "my-project", possiamo eseguire il comando:
lein new my-project

;; Questo creerà una struttura di base per il progetto, che include il file "project.clj" che contiene le informazioni del progetto e la cartella "src/" dove potremmo iniziare a scrivere il nostro codice.

;; Possiamo quindi utilizzare qualsiasi editor di testo o un ambiente di sviluppo integrato (IDE) come Clojure IDE o Emacs per scrivere il codice del nostro progetto. Dopo aver scritto il codice, dobbiamo compilare il nostro progetto utilizzando il comando "lein compile", che genererà i file .class per il nostro codice Clojure.

;; Infine, possiamo eseguire il nostro progetto usando il comando "lein run" che eseguirà il file "main" del nostro progetto.

```

Deep Dive:

In passato, il processo di avvio di un nuovo progetto in Clojure richiedeva più passaggi e configurazioni manuali. Ma grazie all'avvento di Leiningen, tutto il processo è diventato molto più semplice e automatizzato.

Alternative al framework Leiningen includono Maven e Gradle. Tuttavia, molti programmatori preferiscono ancora Leiningen per la sua facilità d'uso e la compatibilità con le librerie e i plugin di Clojure.

Per quanto riguarda l'implementazione di un nuovo progetto in Clojure, il linguaggio è basato su Java e utilizza la JVM (Java Virtual Machine) per eseguire il codice. Quindi, è possibile utilizzare qualsiasi libreria o framework Java all'interno di un progetto Clojure.

Vedi anche:

- [Leiningen](https://leiningen.org/) - sito ufficiale di Leiningen
- [The Clojure Style Guide](https://github.com/bbatsov/clojure-style-guide) - una guida alle best practice per la scrittura di codice Clojure
- [Learn Clojure](https://www.learn-clojure.com/) - una raccolta di risorse per imparare Clojure