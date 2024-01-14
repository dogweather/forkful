---
title:    "Fish Shell: Lettura degli argomenti della riga di comando"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Se sei nuovo alla programmazione di Fish Shell o sei semplicemente interessato a imparare di più su come funzionano gli argomenti della riga di comando, questo post è perfetto per te. Scoprirai quanto sia utile e conveniente leggere gli argomenti della riga di comando nei tuoi script.

## Come Fare

Per leggere gli argomenti della riga di comando in Fish Shell, è sufficiente utilizzare il comando "set" e la variabile speciale "$argv", che contiene una lista di tutte le stringhe passate come argomenti. Ecco un esempio di codice:

```
Fish Shell

set arg_1 $argv[1]
echo "Il primo argomento è $arg_1"
```

Se ad esempio eseguiamo lo script con il comando "fish script.sh ciao", l'output sarà "Il primo argomento è ciao". Come puoi vedere, il comando "set" è stato utilizzato per assegnare la prima stringa dell'array "$argv" alla variabile "arg_1" e poi è stata stampata la stringa con l'uso di una variabile.

## Approfondimento

Esistono molti altri modi per leggere e utilizzare gli argomenti della riga di comando in Fish Shell. Ad esempio, è possibile utilizzare il comando "count" per contare il numero di argomenti passati o il comando "string" per formattare e manipolare le stringhe degli argomenti.

Inoltre, è importante notare che gli argomenti della riga di comando possono essere utilizzati non solo per eseguire operazioni all'interno del tuo script, ma anche per passare informazioni dinamiche all'utente o per creare uno script più flessibile e adattabile.

## Vedi Anche

- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial sull'utilizzo degli argomenti della riga di comando in Fish Shell](https://medium.com/@danilova2105/command-line-arguments-in-fish-shell-18758c07cae4)
- [Altro esempio di lettura degli argomenti della riga di comando in Fish Shell](https://www.linode.com/docs/guides/command-line-arguments-in-fish-shell/)

Grazie per aver letto questo post. Speriamo che ti sia stato utile e che tu possa utilizzare questa conoscenza per aumentare la tua produttività e migliorare i tuoi script con Fish Shell. Continua a esplorare e a divertirti con questo potente strumento!