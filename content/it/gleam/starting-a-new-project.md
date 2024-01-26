---
title:                "Avvio di un nuovo progetto"
date:                  2024-01-20T18:03:35.071924-07:00
model:                 gpt-4-1106-preview
simple_title:         "Avvio di un nuovo progetto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
Iniziare un nuovo progetto in Gleam significa creare una base vuota per costruire il tuo software. Lo facciamo perché partire da zero ci permette di organizzare le idee e le funzionalità secondo le nostre necessità senza vincoli.

## Come fare:
Per cominciare un nuovo progetto in Gleam, usa il comando `gleam new`. Ecco un esempio:

```shell
gleam new mio_fantastico_progetto
```

Questo comando creerà una nuova cartella con il nome del progetto e una struttura iniziale di cartelle e file. Ecco l'output che potresti vedere:

```shell
Your Gleam project "mio_fantastico_progetto" has been successfully created.
The `mio_fantastico_progetto` directory can now be found in your current directory.
```

## Approfondimento:
Gleam è un linguaggio funzionale staticamente tipizzato che compila per la BEAM, la macchina virtuale di Erlang. Nato nel 2018, si distingue per la sua tipizzazione forte e sicurezza nella concorrenza. Quando avvii un nuovo progetto, hai due scelte principali: una libreria reutilizzabile o un'applicazione stand-alone. Alternativamente, potresti iniziare con una libreria come `rebar3` o `mix` se lavori in un ambiente Erlang o Elixir. A differenza di questi, `gleam new` è specifico per progetti Gleam, organizzando le dipendenze e gli strumenti di test in modo più idoneo per il linguaggio.

## Vedi anche:
- GitHub di Gleam per ulteriori esempi e modelli di progetto: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
