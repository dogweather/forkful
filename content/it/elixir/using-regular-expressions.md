---
title:    "Elixir: Utilizzare le espressioni regolari"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Perché
I regular expressions, o espressioni regolari, sono uno strumento fondamentale per ogni programmatore. Grazie alla loro flessibilità e potenza, permettono di riconoscere e manipolare pattern all'interno di stringhe di testo. Questo le rende estremamente utili per una vasta gamma di applicazioni, come il parsing dei dati, la validazione dei campi di input o la sostituzione di parti del testo.

## Come utilizzarle in Elixir
Per utilizzare le regular expressions in Elixir, è necessario importare il modulo `Regex` e utilizzare la funzione `match?`. Ad esempio, se vogliamo cercare la parola "ciao" all'interno di una stringa, possiamo scrivere:

```Elixir
Regex.match?("ciao", "Benvenuto, ciao! ") #=> true
```

Possiamo anche utilizzare i caratteri speciali come `^` per indicare l'inizio della stringa e `$` per indicare la fine, e i quantificatori come `*` per trovare zero o più ripetizioni di un carattere. Ad esempio, se volessimo trovare tutti i numeri all'interno di una stringa, possiamo utilizzare il seguente pattern:

```Elixir
Regex.match?(/\d+/, "La mia età è 27.") #=> true
```

## Approfondimento
Esistono molte funzionalità avanzate delle regular expressions in Elixir, come i gruppi di cattura, le sostituzioni e le modifiche a livello di linguaggio. Inoltre, Elixir offre anche un modulo chiamato `Regex.Scanner` che permette di analizzare le stringhe estrarre facilmente i dati che ci interessano. Per imparare di più su tutte le funzionalità delle regular expressions in Elixir, è consigliato consultare la documentazione ufficiale del linguaggio.

## Vedi anche
- [Documentazione delle regular expressions in Elixir](https://hexdocs.pm/elixir/Regex.html)
- [Tutorial sulle regular expressions in Elixir](https://pragmaticstudio.com/tutorials/elixir-regular-expressions)
- [Elixir School: Regular Expressions](https://elixirschool.com/en/lessons/basics/pattern-matching/#regex)