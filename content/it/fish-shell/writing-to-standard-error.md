---
title:    "Fish Shell: Scrivere su standard error"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error (stderr) è un modo per controllare e gestire gli errori all'interno dei nostri script di Fish Shell. È una pratica utile per assicurarci che i nostri codici funzionino correttamente e per identificare e risolvere eventuali errori.

## Come Fare

In Fish Shell, è possibile scrivere su stderr utilizzando il comando `echo` insieme al simbolo `&2`, che indica che vogliamo scrivere sul canale di errore anziché su standard output (stdout).

```
Fish Shell Funziona? &2 
```

L'output di questo comando sarà "Funziona?" su stderr.

Possiamo anche utilizzare il comando `printf` per scrivere su stderr.

```
Fish Shell printf "Errore: %s" "file non trovato" &2
```

L'output di questo comando sarà "Errore: file non trovato" su stderr.

Questi sono solo due esempi di come scrivere su stderr in Fish Shell, ma ci sono molti altri modi per farlo. Inoltre, è importante notare che è possibile scrivere su stderr anche durante l'esecuzione di una pipe, aggiungendo semplicemente il simbolo `&2` al comando desiderato.

## Approfondimento

Scrivere su standard error è un'importante abilità per ogni programmatore, in quanto ci permette di tracciare e gestire gli errori nei nostri script. Inoltre, ci consente di identificare e risolvere eventuali problemi di esecuzione del codice.

Un'ulteriore utilità di scrivere su stderr è che ci permette di distinguere tra gli output di un programma e gli errori che possono verificarsi durante l'esecuzione dello script. Inoltre, possiamo anche utilizzare lo standard error come modo per informare l'utente che qualcosa non è andato come previsto nel codice.

Inoltre, è importante notare che è possibile reindirizzare lo stderr in un file di log, in modo da poter controllare gli errori dopo l'esecuzione dello script.

## Vedi Anche

- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial di Fish Shell su stderr](https://www.cyberciti.biz/faq/linux-unix-redirect-error-output-to-file/)
- [Esempi di codice su stdout e stderr in Fish Shell](https://gist.github.com/kaushalmodi/1bbae01f05e1fb74b80b63fbb7c1396b)