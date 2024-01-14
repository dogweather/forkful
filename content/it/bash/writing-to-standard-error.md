---
title:    "Bash: Scrittura su standard error"
keywords: ["Bash"]
---

{{< edit_this_page >}}

# Perché scrivere su standard error?

Scrivere su standard error è un'importante abilità per ogni programmatore in Bash. Invece di scrivere messaggi di errore o di debug sullo standard output, scrivere su standard error permette di distinguere chiaramente tra i due tipi di output. Inoltre, scrivere su standard error consente di catturare gli errori e di registrare le informazioni di debug.

## Come scrivere su standard error

Per scrivere su standard error in Bash, è possibile utilizzare il comando ">&2" dopo il comando echo. Ad esempio, se vogliamo scrivere la stringa "Errore!" su standard error, possiamo utilizzare il seguente codice:

```Bash
echo "Errore!" >&2
```

Il comando "&>" può anche essere utilizzato per scrivere sia su standard output che su standard error. Ad esempio, se vogliamo scrivere sia la stringa "Hello" su standard output e la stringa "Errore!" su standard error, possiamo utilizzare il seguente codice:

```Bash
echo "Hello" &> echo "Errore!" >&2
```

## Approfondimento su scrivere su standard error

Scrivere su standard error è particolarmente utile quando si sta eseguendo uno script e si desidera registrare gli errori in un file di log. Con la combinazione dei comandi ">&2" e ">>" è possibile scrivere i messaggi di errore su un file specifico. Ad esempio, il seguente codice salverà tutti i messaggi di errore del comando "ls" in un file di log chiamato "errori.log":

```Bash
ls myfile.txt >> errori.log 2>&1
```

In questo caso, "&1" indica dove deve essere salvato lo standard output e ">&2" indica dove deve essere salvato lo standard error.

# Vedi anche

- https://tldp.org/HOWTO/Bash-Beginners-Guide/html/sect_02_03.html
- https://linuxhandbook.com/bash-redirection/
- https://www.redhat.com/sysadmin/write-stderr-standard-output-bash