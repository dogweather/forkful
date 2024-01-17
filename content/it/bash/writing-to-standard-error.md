---
title:                "Scrivere su errore standard"
html_title:           "Bash: Scrivere su errore standard"
simple_title:         "Scrivere su errore standard"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Scrivere su standard error, o stderr, è un modo per visualizzare gli errori o messaggi di avviso durante l'esecuzione di uno script Bash. I programmatori utilizzano la scrittura su stderr per identificare e risolvere i problemi durante lo sviluppo di script o programmi.

## Come fare:

Di seguito è riportato un esempio di codice Bash che scrive su stderr seguito dal risultato fornito:
```Bash
echo "Messaggio di errore" >&2
```
Output:
```
Messaggio di errore
```

In questo esempio, il messaggio viene scritto su stderr utilizzando `>&2` dopo il comando `echo`.

## Approfondimento:

Scrive su stderr è stato introdotto nell'utilizzo di Unix per consentire ai programmatori di visualizzare messaggi di errore e di warning durante l'esecuzione di script o programmi. Alternativamente, è possibile utilizzare il comando `>/dev/null` per ignorare completamente i messaggi di errore, ma questo può rendere più difficile la risoluzione dei problemi.

Per scrivere su stderr utilizzando l'operatore `>&2`, è necessario comprendere il concetto di redirezione dei flussi di input e output dei comandi in Bash. In questo caso, `>&2` specifica che il risultato del comando deve essere indirizzato al file descriptor 2, che è riservato per stderr.

## Vedi anche:

- Documentazione ufficiale di Bash: https://www.gnu.org/software/bash/
- Redirezione dei flussi in Bash: https://www.gnu.org/software/bash/manual/html_node/Redirections.html