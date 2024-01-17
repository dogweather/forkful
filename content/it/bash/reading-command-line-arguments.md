---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Bash: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cosa & perché?
Leggere gli argomenti della riga di comando è il processo di leggere le informazioni inserite dall'utente nel terminale quando viene eseguito un programma nella shell Bash. I programmatori usano questa funzionalità per ottenere input dagli utenti e personalizzare l'esecuzione del programma.

## Come fare:
Di seguito sono riportati alcuni esempi di codice e l'output corrispondente per illustrare come leggere gli argomenti della riga di comando in Bash.

```Bash
# Esempio 1: Leggere un singolo argomento
# Input: bash sample.sh hello
#!/bin/bash
echo "Il primo argomento è: $1"

# Output: Il primo argomento è: hello

# Esempio 2: Leggere più argomenti
# Input: bash sample.sh hello world
#!/bin/bash
echo "Ciao a tutti i miei amici: $1 e $2"

# Output: Ciao a tutti i miei amici: hello e world
```

## Approfondimento:
### Contesto storico:
Il concetto di leggere gli argomenti della riga di comando è stato introdotto negli anni '70 con l'arrivo delle prime shell e si è evoluto nel tempo, diventando una pratica comune tra i programmatori.

### Alternative:
Oltre a Bash, ci sono altri linguaggi di scripting che supportano la lettura degli argomenti della riga di comando, come Python e Perl.

### Dettagli implementativi:
Per accedere agli argomenti della riga di comando in Bash, si utilizza il simbolo "$" seguito dal numero dell'argomento desiderato (iniziando da 1). Inoltre, è possibile utilizzare l'array "@", che contiene tutti gli argomenti inseriti.

## Vedi anche:
- [Documentazione ufficiale Bash](https://www.gnu.org/software/bash/)
- [Esempi di letture degli argomenti della riga di comando](https://www.tutorialspoint.com/unix_commands/bash.htm)
- [Articolo su esempi di argomenti della riga di comando in Bash](https://www.linuxnix.com/reading-command-line-arguments-bash-script/)