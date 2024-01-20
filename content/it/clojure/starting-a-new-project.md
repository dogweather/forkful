---
title:                "Iniziare un nuovo progetto"
html_title:           "Arduino: Iniziare un nuovo progetto"
simple_title:         "Iniziare un nuovo progetto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Iniziare un nuovo progetto in programmazione significa creare un nuovo ambiente di lavoro, tutto pulito e carico di nuove opportunità. Lo facciamo per realizzare idee, creare nuovi software, o semplicemente per imparare nuove tecnologie.

## Come Fare:

Per iniziare un nuovo progetto in Clojure, utilizziamo Leiningen (un famoso strumento di gestione di progetti Clojure), eseguendo il seguente comando nell'interfaccia a linea di comando:

```Clojure
lein new nome-del-progetto
```

Questo genererà una nuova directory con lo stesso nome del progetto contenente altri file e directory, tra cui `project.clj` (il cuore del tuo progetto) e `src/nome_del_progetto/core.clj` (il tuo principale file sorgente Clojure).

Per eseguire il tuo progetto, entra nella directory del progetto e esegui:

```Clojure
lein run
```

L'output sarà qualcosa di simile a:

```Clojure
Hello, World!
```

## Approfondimento:

Clojure è un linguaggio funzionale moderno nato nel 2007, creato da Rich Hickey. È basato su Lisp ma consuma la JVM (Java Virtual Machine), il che lo rende un'alternativa popolare ad altri linguaggi JVM come Java o Scala.

Ci sono alternative a Leiningen, come Boot o Clojure CLI, tuttavia, Leiningen rimane lo standard del settore per Clojure grazie alla sua facilità d'uso e alla sua vasta comunità.

Implementare un nuovo progetto in Clojure richiede una buona conoscenza di Clojure (logicamente!), una comprensione della filosofia funzionale che sottosta il linguaggio, e una consapevolezza degli strumenti e delle librerie disponibili.

## Vedi Anche:

1. [Leiningen Homepage](https://leiningen.org/)
2. [Clojure CLI](https://clojure.org/guides/getting_started)
4. [Impara Clojure in Y minuti](https://learnxinyminutes.com/docs/clojure/)
5. [Introduzione a Clojure - Video Corso](https://www.youtube.com/watch?v=VSdnJDO-xdg)