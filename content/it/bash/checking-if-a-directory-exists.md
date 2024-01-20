---
title:                "Verificare se una directory esiste"
html_title:           "Bash: Verificare se una directory esiste"
simple_title:         "Verificare se una directory esiste"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
Rendere il codice robusto ed evitare errori involontari è fondamentale nella programmazione. Una pratica comune è verificar se una directory esiste prima di utilizzarla, per evitare errori dovuti al tentativo di accedere o manipolare una directory inesistente.

## Come si fa:
Ecco un semplice esempio su come verificare se una directory esiste in Bash.

```Bash
if [ -d "$DIRECTORY" ]; then
  echo "La directory esiste."
else
  echo "La directory non esiste."
fi
```
Dove `$DIRECTORY` è il percorso della directory da controllare. L'output sarà "La directory esiste." se la directory esiste o "La directory non esiste." se la directory non esiste.

## Approfondimenti
Il comando `-d` usato nel codice di cui sopra è un operatore di test che ritorna vero (`true`) se la directory esiste, ed è disponibile in Bash dalla sua prima versione.

Ci sono diversi modi per ottenere lo stesso risultato in Bash, ad esempio usando il comando `stat` o `test`. Il comando `test` è equivalente all'uso di parentesi quadre (`[ ]`) mentre `stat` è un po' più versatile e offre anche altre informazioni sulla directory.

Ricorda sempre di gestire il caso in cui la directory non esiste nel tuo codice, in modo da evitare errori a runtime. Ecco un esempio di come ciò potrebbe apparire nel codice:

```Bash
if [ -d "$DIRECTORY" ]; then
  echo "La directory esiste."
else
  echo "La directory non esiste."
  mkdir $DIRECTORY
fi
```
Qui, se la directory non esiste, la creiamo con il comando `mkdir`.

## Guarda Anche
Oltre al comando `-d`, ci sono molti altri operatori di test disponibili in Bash, che possono essere molto utili. Puoi trovare una lista completa in questa [guida di Bash](https://www.gnu.org/software/bash/manual/bash.html#Bash-Conditional-Expressions).

Se vuoi approfondire la creazione di directory in Bash, guarda questo [tutorial](https://linuxize.com/post/bash-mkdir-command/).

Se vuoi sapere di più sulle basi del linguaggio di programmazione Bash, ti consiglio questa [guida introduttiva](https://linuxize.com/post/bash-scripting-tutorial/).