---
title:                "Bash: Verifica dell'esistenza di una directory."
simple_title:         "Verifica dell'esistenza di una directory."
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

La verifica dell'esistenza di una directory è un'operazione fondamentale nella programmazione di Bash. La capacità di determinare se una directory specifica esiste o meno può essere utile in molte situazioni, come ad esempio durante la creazione di script di automazione o nella gestione di file.

## Come fare

Per verificare se una directory esiste utilizzando Bash, è possibile utilizzare il comando `test` con l'opzione `-d` seguito dal percorso della directory desiderata. Ad esempio:

```Bash
test -d /home/utente/documenti
```

Se la directory esiste, il comando restituirà un valore "vero" (0). Altrimenti, se la directory non esiste o non è accessibile, il comando restituirà un valore "falso" (1).

Inoltre, è possibile anche utilizzare l'operatore di controllo dei flussi `if`, `then` e `fi` per gestire le azioni da compiere in base al risultato della verifica. Un esempio di codice sarebbe il seguente:

```Bash
if test -d /home/utente/documenti; then
  echo "La directory esiste!"
else
  echo "La directory non esiste."
fi
```

Nell'esempio sopra, se la directory esiste, verrà stampato il messaggio "La directory esiste!". In caso contrario, verrà stampato il messaggio "La directory non esiste.".

## Approfondimento

Per verificare se una directory esiste, Bash esegue in realtà un controllo sui metadati del file. In particolare, il controllo avviene sul bit "directory" dei metadati che viene impostato solo se il file in questione è effettivamente una directory.

È importante notare che la verifica dell'esistenza di una directory mediante il comando `test` non garantisce che la directory non verrà eliminata o spostata in un secondo momento. Pertanto, è importante assicurarsi che il codice migliori venga protetto da eventuali errori dovuti alla non esistenza della directory.

## Vedi anche

- [Documentazione ufficiale di Bash](https://www.gnu.org/software/bash/manual/)
- [Articolo di Wikipedia su Bash](https://it.wikipedia.org/wiki/Bash)
- [Tutorial su Scripting in Bash](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)