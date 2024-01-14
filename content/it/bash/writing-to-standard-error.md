---
title:    "Bash: Scrivere su standard error"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error può sembrare una piccola e insignificante parte della programmazione, ma in realtà può essere estremamente utile. Invece di stampare messaggi di errore sullo standard output, scrivendoli su standard error possiamo separare i messaggi di errore dal normale output del nostro programma. Ciò ci permette di visualizzare solo i messaggi di errore in caso di crash o problemi durante l'esecuzione del programma.

## Come

Per scrivere su standard error in Bash, utilizziamo il comando `>&2` dopo il messaggio o l'output che vogliamo inviare. Ad esempio:

```Bash
echo "Errore: file non trovato" >&2
```
Questo comando invierà il messaggio "Errore: file non trovato" su standard error invece che sullo standard output.

Possiamo anche utilizzare `&>file` per inviare sia lo standard output che lo standard error su un file specifico. Ad esempio:

```Bash
ls /directory/inventata &>log.txt
```

## Deep Dive

Scrivere su standard error non solo ci permette di separare i messaggi di errore dal normale output, ma ha anche altre utili applicazioni. Per esempio, possiamo utilizzarlo per loggare i messaggi di debug durante lo sviluppo di un programma. I messaggi di debug verranno inviati su standard error e così non interferiranno con l'output finale del programma.

Possiamo anche utilizzare un file di log specifico su cui scrivere i messaggi di errore, in modo da poterli controllare successivamente in caso di problemi durante l'esecuzione del programma.

## See Also

Per ulteriori informazioni sul comando `>&2` e sulle sue applicazioni, consiglio di leggere la documentazione ufficiale di Bash: [Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html#Redirections)

Altre risorse utili possono essere trovate su:

- [Stack Overflow](https://stackoverflow.com/questions/tagged/bash)
- [Bash-it](https://github.com/Bash-it/bash-it)
- [Shell Scripting Tutorial](https://www.shellscript.sh/)