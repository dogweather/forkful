---
title:                "Scrivere sull'errore standard"
date:                  2024-01-19
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"

category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Scrivere nello standard error (stderr) serve a segnalare errori o messaggi diagnostici separandoli dall'output standard (stdout). I programmatori lo usano per facilitare il debug e il logging degli errori senza intaccare l'output primario del programma.

## Come fare:

Per redirigere l'output di errore in Bash, si utilizza `>&2`. Ecco un esempio:

```Bash
echo "Questo va allo standard output"
echo "Questo è un messaggio di errore" >&2
```

Output:

```
Questo va allo standard output
Questo è un messaggio di errore
```

Nell'output del terminale non dovrebbero esserci differenze visive, ma il messaggio di errore verrà inviato allo stderr.

## Approfondimento:

Lo standard error (file descriptor 2) è stato introdotto con le prime versioni di Unix per differenziare l'output normale dall'output di errore. È possibile redirigere stdout e stderr in file diversi o unificarli usando `2>&1`. In contesti come script cron, dove lo stdout potrebbe non essere visibile, scrivere sullo stderr assicura che i messaggi di errore siano notati.

## Vedi anche:

- [Bash Redirections Cheat Sheet](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)
- [Advanced Bash-Scripting Guide: I/O Redirection](http://www.tldp.org/LDP/abs/html/io-redirection.html)
- [Greg's Wiki: BashFAQ - Error handling](https://mywiki.wooledge.org/BashFAQ/105)
