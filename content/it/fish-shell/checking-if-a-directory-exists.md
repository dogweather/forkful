---
title:                "Verifica dell'esistenza di una directory"
html_title:           "Arduino: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
In Fish Shell, controllare se una directory esiste significa verificare la presenza di una cartella prima di operarvi. Lo facciamo per evitare errori durante la manipolazione dei file o per garantire che un certo ambiente o le risorse siano presenti.

## How to:
Per verificare se una directory esiste, usa il comando `test` e poi `echo` per la risposta, oppure `if` per eseguire codice condizionale.

```Fish Shell
if test -d /percorso/alla/directory
    echo "La directory esiste."
else
    echo "La directory non esiste."
end
```

Output:
```
La directory esiste.
```

## Deep Dive
Prima dell'avvento dei sistemi moderni, controllare l'esistenza di una directory poteva richiedere script complessi. In Fish Shell, il built-in `test` semplifica questo processo. Alcune alternative comprendono l'uso di `stat` o di altri comandi esterni come `ls`, ma `test -d` è il metodo consigliato per via della sua semplicità e leggibilità. 

Quando usi `test -d`, stai chiedendo al sistema se il percorso indicato è una directory e se questa è accessibile. È un'operazione comune e fondamentale in script che automatizzano task su filesystem multiutente o in contesti di sviluppo in cui le directories devono essere create e verificate dynamicamente.

## See Also
- La documentazione ufficiale di Fish Shell su `test`: https://fishshell.com/docs/current/commands.html#test
- Best practices per scripting Fish Shell: https://fishshell.com/docs/current/tutorial.html
- Guide alla programmazione della shell: https://mywiki.wooledge.org/BashGuide