---
title:                "Verifica dell'esistenza di una cartella"
html_title:           "Bash: Verifica dell'esistenza di una cartella"
simple_title:         "Verifica dell'esistenza di una cartella"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché
In Bash, è comune dover verificare se una directory esiste o meno prima di eseguire determinate operazioni su di essa, come ad esempio creare un nuovo file o spostare file all'interno della directory. In questo articolo vedremo come effettuare questa operazione in modo semplice ed efficiente.

## Come fare
```Bash
# Utilizziamo il comando "test" seguito dalla flag "-d" per specificare che vogliamo verificare se una directory esiste
test -d my_directory
# Questo comando restituirà un valore "0" se la directory esiste e un valore diverso da "0" se non esiste
# Possiamo quindi utilizzare la sintassi "if" per eseguire determinate azioni in base al risultato della verifica
if [ $? -eq 0 ]
then
  # La directory esiste, possiamo eseguire le nostre operazioni
else
  # La directory non esiste, possiamo gestire l'errore
fi
```

## Approfondimento
È importante notare che il comando "test" può anche essere scritto come due parentesi quadre "[ ]" e che il seguito "-eq" significa "uguale a". Inoltre, possiamo utilizzare la flag "-f" per verificare se un file esiste invece che una directory.

## Vedi anche
- Guida di riferimento su Bash: https://www.gnu.org/software/bash/
- Documentazione del comando "test": https://www.gnu.org/software/coreutils/manual/html_node/test-invocation.html