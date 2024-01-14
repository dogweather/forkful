---
title:    "Fish Shell: Verifica dell'esistenza di una cartella"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Perché

Controllare l'esistenza di una directory è un'operazione fondamentale nella programmazione di shell. Può essere utile per garantire che il percorso specificato sia corretto e per gestire eventuali errori.

## Come fare

Per verificare l'esistenza di una directory, è possibile utilizzare il comando "test" con l'opzione "-d" seguita dal percorso della directory che si desidera controllare. Ad esempio:

```Fish Shell
test -d /usr/bin
```

Se la directory esiste, il comando restituirà un valore di uscita di 0, altrimenti restituirà un valore diverso da 0.

Per gestire l'output del comando, è possibile utilizzare una condizione if/else, ad esempio:

```Fish Shell
if test -d /usr/bin
    echo "La directory esiste"
else
    echo "La directory non esiste"
end
```

Se si desidera verificare l'esistenza di una directory in una posizione relativa, è possibile utilizzare il comando "set -q" seguito dal nome della variabile contenente il percorso della directory. Ad esempio:

```Fish Shell
set -q MY_DIR
if test -d $MY_DIR
    echo "La directory esiste"
else
    echo "La directory non esiste"
end
```

## Approfondimento

Il comando "test" è un'abbreviazione per il comando "builtin test", che è uno dei comandi di base forniti dal Fish Shell. Questo comando può essere utilizzato per controllare l'esistenza di diversi tipi di file, non solo di directory. Per ulteriori informazioni su come utilizzarlo, è possibile consultare la documentazione ufficiale del Fish Shell.

## Vedi anche

- [Documentazione ufficiale del Fish Shell](https://fishshell.com/docs/current/index.html)
- [Esempi di utilizzo del comando test](https://linuxize.com/post/check-if-a-directory-exists-in-bash/)