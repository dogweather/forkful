---
title:                "Fish Shell: Controllare se una cartella esiste"
simple_title:         "Controllare se una cartella esiste"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Perché
Controllare se una directory esiste è una pratica comune nella programmazione, specialmente se si lavora con file e cartelle nella gestione dei dati. Può aiutare a evitare errori e garantire che il codice funzioni correttamente.

## Come Fare
Per verificare se una directory esiste, si può utilizzare il comando `test -d` nel Fish Shell. Questo comando restituirà `0` se la directory esiste oppure `1` se non esiste. Ecco un esempio di codice:

```
if test -d ./documents
    echo "La directory documents esiste!"
else
    echo "La directory documents non esiste."
end
```

In questo esempio, se la directory "documents" esiste, verrà stampato il messaggio `La directory documents esiste!`, altrimenti verrà stampato `La directory documents non esiste.`.

## Approfondimento
Quando si utilizza il comando `test -d`, si sta in realtà utilizzando il comando `test` con l'opzione `-d`, che sta per "directory". Questo comando può essere utilizzato anche per controllare se esistono o meno file o altre informazioni di sistema. Inoltre, se si utilizza l'opzione `-e` al posto di `-d`, si può verificare semplicemente l'esistenza di un file o di qualsiasi altro tipo di informazione.

## Vedi Anche
- [Documentazione Fish Shell - test](https://fishshell.com/docs/current/cmds/test.html)
- [Tutorial di Fish Shell su YouTube - Controllo se una directory esiste](https://www.youtube.com/watch?v=vSWrqr-oqIY)
- [Articolo Medium - Connessione alle directory in Fish Shell](https://medium.com/@matthiasspiess/connecting-to-directories-in-fish-shell-6c3dcbb22e30)