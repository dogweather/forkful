---
title:    "Gleam: Iniziare un nuovo progetto"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Perché iniziare un nuovo progetto con Gleam?

Se sei un programmatore alla ricerca di un linguaggio funzionale moderno e performante, allora Gleam è la scelta perfetta per te. Con la sua sintassi user-friendly e la potenza della compilazione statica, questo linguaggio ti permetterà di scrivere codice efficiente e robusto per i tuoi progetti.

# Come iniziare

Per iniziare a sfruttare il potenziale di Gleam, basta seguirere pochi semplici passi. Prima di tutto, assicurati di avere installato sul tuo computer l'ultima versione di Erlang/OTP e di Elixir.

Una volta completata l'installazione, puoi creare un nuovo progetto Gleam con il seguente comando nel terminale:

```Gleam new nome-progetto```

Questo comando creerà una struttura di base per il tuo progetto composta da una cartella "src" contenente il file "main.gleam" e una cartella "test" contenente un file di test di esempio.

Per compilare il tuo progetto, utilizza il seguente comando:

```Gleam build```

E per eseguire il tuo codice, usa il comando:

```Gleam run```

Il risultato verrà visualizzato direttamente nel terminale.

# Approfondimento

Per iniziare a scrivere codice Gleam, è importante comprendere i principi fondamentali del linguaggio. Gleam utilizza la sintassi funzionale e la programmazione basata sui tipi, il che significa che tutte le espressioni e le funzioni devono avere un tipo specificato.

Inoltre, Gleam utilizza il concetto di moduli per strutturare il codice. Un modulo è una collezione di dichiarazioni, funzioni e tipi, che possono essere importati ed utilizzati in altri moduli. Questo permette una maggiore organizzazione e modularità nel tuo codice.

Infine, Gleam ha un forte sistema di tipo e un sistema di gestione degli errori robusto che aiuta a prevenire errori comuni durante la compilazione e l'esecuzione del tuo codice.

# Vedi anche

- Documentazione ufficiale di Gleam: https://gleam.run/
- Repository GitHub di Gleam: https://github.com/gleam-lang/gleam
- Tutorial di introduzione a Gleam: https://dev.to/cscott/functional-programming-in-gleam-5cdi