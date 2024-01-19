---
title:                "Iniziare un nuovo progetto"
html_title:           "Arduino: Iniziare un nuovo progetto"
simple_title:         "Iniziare un nuovo progetto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Avviare un nuovo progetto significa iniziare a sviluppare un nuovo software o applicazione da zero. I programmatori lo fanno per risolvere problemi specifici, sperimentare nuove idee o migliorare le competenze tecniche.

## Come fare:

Ecco un esempio di come creare una nuova directory per il tuo progetto e cambiarla come directory di lavoro attuale nel Fish Shell.

```Fish Shell
> mkdir nuovo_progetto
> cd nuovo_progetto
```

Puoi anche creare un semplice script shell per automatizzare il processo.

```Fish Shell
> function nuovo_progetto
    mkdir $argv
    cd $argv
end
> nuovo_progetto mio_progetto
```

Nell'output di esempio, creiamo una funzione `nuovo_progetto` che prende un argomento per il nome del progetto, crea una nuova directory con quel nome e la imposta come la directory di lavoro attuale.

## Approfondimento

Fish Shell è un linguaggio di scripting moderno che si basa su shell Unix tradizionali come bash o zsh. Ha una sintassi semplificata e include funzioni di utilità per facilitare la scrittura di script.

Altre alternative per avviare un nuovo progetto potrebbero includere l'utilizzo di template preimpostati o generatori di scaffolding come Yeoman.

Un dettaglio di implementazione importante da considerare durante l'avvio di un nuovo progetto è la struttura della directory. E potrebbe essere utile impostare un sistema di controllo versione come Git sin dall'inizio per tracciare i cambiamenti nel codice.

## Vedi anche

Per ulteriori dettagli e tutorial su Fish Shell, visita questi link:

- Documentazione ufficiale di Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Introduzione a Fish Shell: [https://opensource.com/article/19/1/productivity-tool-fish-shell](https://opensource.com/article/19/1/productivity-tool-fish-shell)
- Guida per iniziare con Yeoman: [https://yeoman.io/learning/index.html](https://yeoman.io/learning/index.html)