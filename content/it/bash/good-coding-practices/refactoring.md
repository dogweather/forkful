---
title:                "Rifattorizzazione"
date:                  2024-01-26T01:16:24.647673-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rifattorizzazione"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/refactoring.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Il refactoring è il processo di ristrutturazione del codice informatico esistente senza cambiarne il comportamento esterno. È una pratica fondamentale per ridurre la complessità, migliorare la manutenibilità e mantenere il tuo codice sano e più facile da comprendere sia per gli sviluppatori attuali sia per quelli futuri.

## Come fare:
Consideriamo uno script Bash semplice che necessita di un po' di refactoring. È goffo, con codice ripetuto ed è difficile da seguire:

```Bash
#!/bin/bash
echo "Inserisci un nome di file:"
read filename
if [ -f "$filename" ]; then
    echo "Il file esiste."
    count=$(grep -c "foo" "$filename")
    echo "La parola foo appare $count volte."
else
    echo "Il file non esiste."
fi
```

Il refactoring per chiarezza e riusabilità potrebbe comportare l'introduzione di funzioni e la gestione degli errori in modo più elegante:

```Bash
#!/bin/bash

function file_exists() {
    [[ -f "$1" ]]
}

function count_occurrences() {
    grep -c "$1" "$2"
}

function main() {
    local filename word count
    echo "Inserisci un nome di file:"
    read -r filename
    echo "Inserisci la parola da cercare:"
    read -r word

    if file_exists "$filename"; then
        count=$(count_occurrences "$word" "$filename")
        echo "La parola $word appare $count volte."
    else
        echo "Il file non esiste." >&2
        exit 1
    fi
}

main "$@"
```

La versione rifattorizzata utilizza le funzioni per migliorare la leggibilità e consente un potenziale riutilizzo.

## Approfondimento:
Il refactoring non è un concetto nato con Bash o persino con i linguaggi di programmazione di alto livello; è vecchio quanto la programmazione stessa. Il termine è stato formalizzato nel libro "Refactoring: Improving the Design of Existing Code" di Martin Fowler nel 1999, concentrando l'attenzione principalmente sui linguaggi orientati agli oggetti.

Nel contesto degli script Bash, il refactoring spesso significa suddividere script lunghi in funzioni, ridurre la ripetizione con cicli o condizionali ed evitare trappole comuni come il mancato trattamento degli spazi nei nomi dei file. Alternative a Bash per script diventati troppo complessi includono Python o Perl, che offrono migliori strutture dati e gestione degli errori per compiti complessi.

Il refactoring specifico di Bash riguarda più l'aderire alle migliori pratiche, come mettere tra virgolette le variabili, usare `[[ ]]` per i test invece di `[ ]`, e preferire `printf` a `echo` per un output robusto. I dettagli di implementazione spesso si concentrano sull'aderire alle guide di stile e sull'utilizzo di strumenti come `shellcheck` per l'analisi statica al fine di individuare errori comuni.

## Vedi Anche:
- [Google's Shell Style Guide](https://google.github.io/styleguide/shellguide.html)
- [ShellCheck, uno strumento di analisi statica per script shell](https://www.shellcheck.net/)
- [The Art of Command Line](https://github.com/jlevy/the-art-of-command-line)
