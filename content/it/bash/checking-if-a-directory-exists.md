---
title:    "Bash: Verifica dell'esistenza di una directory"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Perché controllare se una directory esiste?

Controllare se una directory esiste è un'operazione comune nella programmazione Bash. Può essere utile per verificare se una certa cartella è presente prima di eseguire un'operazione su di essa, come ad esempio creare un nuovo file o copiare dei file al suo interno. Inoltre, può aiutare a gestire gli errori nel caso in cui una directory non esista.

## Come fare?

Per controllare se una directory esiste in Bash, è possibile utilizzare il comando `test` seguito dall'opzione `-d`, che indica che si sta verificando un elemento di tipo directory. Inoltre, è necessario specificare il percorso completo della directory che si desidera controllare.

Ecco un esempio di codice che verifica se la directory "Documenti" esiste nella home directory dell'utente corrente:

```Bash
if test -d ~/Documenti; then
    echo "La directory Documenti esiste."
else
    echo "La directory Documenti non esiste."
fi
```

In questo caso, il comando `test` restituirà un valore vero se la directory esiste e un valore falso se non esiste. Utilizzando un semplice condizionale `if`, è possibile determinare se la directory esiste o meno e quindi eseguire le azioni appropriate.

## Approfondimento

Esistono altre opzioni del comando `test` che possono essere utili per il controllo delle directory. Ad esempio, l'opzione `-e` può essere utilizzata per controllare l'esistenza di qualsiasi tipo di file o directory, mentre l'opzione `-f` controlla specificamente se si tratta di un file regolare.

Inoltre, è importante notare che il comando `test` utilizza la sintassi a doppio colchetto `[[ ]]` invece della sintassi a singolo colchetto `[ ]` quando viene utilizzato con le opzioni `-d`, `-e` e `-f`.

## Vedi anche

Se vuoi saperne di più sulla gestione dei file e delle directory in Bash, puoi consultare questi utili link:

- [La Guida Pratica Bash: Debian](https://www.debian.org/doc/manuals/debian-reference/ch01.en.html#_basics_of_file_operations)
- [Tutorial su Shell Scripting Bash](https://www.shellscript.sh/index.html)
- [Come verificare se un file esiste nel Bash Scripting](https://linuxize.com/post/bash-check-if-file-exists/)