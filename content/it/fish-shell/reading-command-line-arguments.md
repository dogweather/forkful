---
title:                "Fish Shell: Leggere gli argomenti della riga di comando"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Il mondo della programmazione è vasto e ricco di strumenti e linguaggi. Tra questi, la Shell è sicuramente uno dei più utilizzati ed importanti per gli sviluppatori. Ma cos'è la Shell? In parole semplici, la Shell è un'interfaccia di linea di comando che permette agli utenti di interagire con il sistema operativo e di eseguire comandi ed operazioni direttamente dal terminale. In questo articolo, parleremo di una funzionalità molto utile e spesso utilizzata nella Shell: la lettura degli argomenti da riga di comando.

## Come fare

Per leggere gli argomenti da riga di comando in Fish Shell, è necessario utilizzare la variabile `$argv`. Questa variabile contiene un array con tutti gli argomenti passati al momento dell'esecuzione dello script o del comando. Ecco un esempio di come utilizzarla:

```Fish Shell
#!/usr/bin/env fish

# Esempio di script che legge argomenti da riga di comando
set args $argv

# Verifica se sono stati passati almeno due argomenti
if test (count $args) -lt 2
    echo "Sono richiesti almeno due argomenti!"
    exit 1
end

# Stampa il primo e il secondo argomento
echo "Il primo argomento è: $args[1]"
echo "Il secondo argomento è: $args[2]"
```

Se eseguiamo uno script contenente il codice sopra riportato e gli passiamo due argomenti, ad esempio "ciao" e "mondo", otterremo il seguente output:

```
Il primo argomento è: ciao
Il secondo argomento è: mondo
```

## Approfondimento

Oltre alla variabile `$argv`, Fish Shell offre altri modi per leggere ed elaborare gli argomenti passati dalla riga di comando. Ad esempio, possiamo utilizzare il comando `argparse` per definire opzioni e argomenti da passare allo script. Inoltre, possiamo anche utilizzare la funzione `startswith` per verificare se un argomento inizia con una determinata stringa.

Se vuoi saperne di più sull'utilizzo degli argomenti da riga di comando in Fish Shell, ti consigliamo di consultare la documentazione ufficiale della Shell e di sperimentare con diversi esempi.

## Vedi anche

- [Documentazione ufficiale di Fish Shell] (https://fishshell.com/docs/current/index.html#command-line-arguments)
- [Tutorial su argparse in Fish Shell] (https://dev.to/khoaofgod/fish-shell-argparse-57cn)
- [Articolo su startswith in Fish Shell] (https://newbedev.com/how-to-check-if-a-command-line-argument-starts-with-a-given-string)

Grazie per aver letto questo articolo e speriamo che ti sia stato utile per comprendere meglio come leggere gli argomenti da riga di comando in Fish Shell. Continua a seguire la nostra sezione di programmazione per scoprire altri consigli e trucchi utili!