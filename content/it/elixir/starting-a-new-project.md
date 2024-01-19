---
title:                "Iniziare un nuovo progetto"
html_title:           "Arduino: Iniziare un nuovo progetto"
simple_title:         "Iniziare un nuovo progetto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
Avviare un nuovo progetto significa creare una base vuota da cui sviluppare il tuo software, ciò viene fatto per organizzare i file del codice in una struttura standardizzata. I programmatori lo fanno per ottenere un ambiente coerente e facilmente gestibile.

## Come Fare:
Per avviare un nuovo progetto in Elixir, usiamo Mix, un tool built-in di Elixir per la gestione dei progetti. Avviarlo è semplice come inventare il nome del tuo progetto e correre un comando Mix. Ecco l'esempio:

```
Elixir
mix new my_project
```

Avrai quindi l'output:

```
Elixir
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/my_project.ex
* creating test
* creating test/test_helper.exs
* creating test/my_project_test.exs

Your Mix project was created successfully.
```

In questo modo, otterrai un nuovo progetto con una struttura di directory standardizzata e un file mix.exs per la configurazione del progetto.

## Approfondimento:
La creazione di un nuovo progetto è un concetto fondamentale, in qualsiasi linguaggio di programmazione. L'uso di Mix in Elixir risale alla sua creazione nel 2012. Le alternative a Mix includono Rebar3 per Erlang o Leiningen per Clojure, ma Mix è diventato lo strumento di gestione di progetti standard per Elixir. 

La creazione di un nuovo progetto con Mix ti dà un progetto Elixir pronto all'uso con una configurazione standard, incluso un semplice modulo .ex come punto di partenza. Avviare un progetto con Mix significa iniziare con una solida base.

## Vedi Anche:
Per un approfondimento su Mix, consulta la documentazione ufficiale di Elixir [qui](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html).

Per una discussione più ampia su come avviare nuovi progetti in diversi linguaggi di programmazione, vedi [questo](https://www.freecodecamp.org/news/how-to-start-a-new-coding-project/) articolo di FreeCodeCamp. 

Per apprendere di più su Elixir, consulta la guida introduttiva [qui](https://elixir-lang.org/learning.html).