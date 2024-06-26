---
date: 2024-01-20 18:03:11.058697-07:00
description: 'Come Fare: Per iniziare, avrai bisogno di Mix, lo strumento di build
  che viene con Elixir. Ecco i passaggi chiave.'
lastmod: '2024-03-13T22:44:43.084287-06:00'
model: gpt-4-1106-preview
summary: Per iniziare, avrai bisogno di Mix, lo strumento di build che viene con Elixir.
title: Avvio di un nuovo progetto
weight: 1
---

## Come Fare:
Per iniziare, avrai bisogno di Mix, lo strumento di build che viene con Elixir. Ecco i passaggi chiave:

```Elixir
# Installa Elixir se non l'hai già fatto
$ sudo apt-get install elixir

# Dopo aver installato Elixir, crea un nuovo progetto Mix
$ mix new nome_del_progetto

# Vai alla cartella del progetto ed eseguilo
$ cd nome_del_progetto
$ mix run
```

Il risultato sarà una nuova cartella con una struttura di base per il tuo progetto Elixir, pronta per essere popolata con moduli e funzioni.

## Approfondimento:
Elixir è un linguaggio di programmazione funzionale, concorrente, e general-purpose basato sulla Erlang VM (BEAM). Creare progetti con Mix, introdotto da José Valim nel 2011, semplifica il processo di gestione delle dipendenze e la compilazione del codice. Altre alternative includono l'uso di strumenti come Erlang’s rebar o semplicemente organizzare manualmente i tuoi script. Tuttavia, Mix è strettamente integrato con l'ecosistema Elixir, fornendo convenzioni e strumenti che possono accelerare lo sviluppo. Con Mix, puoi facilmente gestire task personalizzati, gestire dipendenze, eseguire test ed altro ancora.

## Vedi Anche:
- Documentazione ufficiale di Mix: [https://hexdocs.pm/mix/Mix.html](https://hexdocs.pm/mix/Mix.html)
- Come installare Elixir: [https://elixir-lang.org/install.html](https://elixir-lang.org/install.html)
- Elixir School, per iniziare con Elixir: [https://elixirschool.com/it/lessons/basics/mix/](https://elixirschool.com/it/lessons/basics/mix/)
