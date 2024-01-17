---
title:                "Iniziare un nuovo progetto"
html_title:           "Gleam: Iniziare un nuovo progetto"
simple_title:         "Iniziare un nuovo progetto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Che cos'è e Perché?
 Iniziare un nuovo progetto significa creare un'ambiente virtuale dove è possibile scrivere codice e sviluppare una nuova applicazione o libreria. I programmatori lo fanno per organizzare il loro lavoro e mantenere le cose ordinate.

## Come fare:

Per iniziare un nuovo progetto in Gleam, è necessario creare una cartella vuota e all'interno creare un file `gleam.toml`. Questo file contiene le informazioni di base sul progetto, come ad esempio il nome, la versione, gli autori e le dipendenze. Un esempio di file toml è il seguente:

```Gleam
[package]
name = "my_project"
version = "1.0.0"
authors = ["John Doe", "Jane Smith"]

[dependencies]
gleam = {version = "0.12.0"}
```

Una volta creato il file `gleam.toml`, è possibile aggiungere altri file al vostro progetto, come ad esempio il file principale `main.gleam` che conterrà il codice in Gleam. Per compilare il progetto, è necessario eseguire il comando ```gleam build```, che creerà un file `.beam` contenente il tuo codice pronto per essere eseguito.

## Deep Dive:

In passato, i programmatori dovevano utilizzare ambienti di sviluppo molto complessi per iniziare un nuovo progetto. Con Gleam, invece, è possibile utilizzare un semplice file toml per specificare le dipendenze e iniziare a codificare immediatamente.

In alternativa, esistono altri linguaggi di programmazione che consentono di iniziare progetti in modo simile, come ad esempio Rust o Elixir. Tuttavia, Gleam offre un sistema di tipi fortemente statico, rendendo più facile scrivere e mantenere il codice nel lungo periodo.

## Vedi anche:

- Documentazione ufficiale di Gleam: https://gleam.run/
- Repository GitHub di Gleam: https://github.com/gleam-lang/gleam