---
title:                "Fish Shell: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Molte volte ci troviamo a lavorare con stringhe di testo, e spesso dobbiamo conoscere la loro lunghezza per eseguire alcune operazioni. In questo articolo spiegherò come utilizzare Fish Shell per ottenere la lunghezza di una stringa in modo facile e veloce.

## Come fare

Per trovare la lunghezza di una stringa in Fish Shell, possiamo utilizzare il comando `len`. Vediamo un esempio:

```
Fish Shell

set stringa "Ciao a tutti"
echo (len $stringa)
```

L'output di questo codice sarà `13`. Prima di tutto, abbiamo creato una variabile `stringa` contenente il testo "Ciao a tutti". Poi, abbiamo utilizzato il comando `len` per ottenere la lunghezza di questa stringa e stamparla con `echo`. Come puoi vedere, è molto semplice!

Ma adesso potresti chiederti, come funziona il comando `len`? Innanzitutto, è importante sapere che `len` è un comando incorporato in Fish Shell, il che significa che non è necessario installare nulla per utilizzarlo.

## Approfondimento

Il comando `len` restituisce la lunghezza della stringa escludendo il carattere di fine riga, se presente. Questo significa che i caratteri speciali come lo spazio e il tab sono inclusi nella lunghezza.

Inoltre, `len` può essere utilizzato anche per ottenere la lunghezza di un array o di un elenco di valori. Ad esempio:

```
Fish Shell

set numeri 1 2 3 4 5 
echo (len $numeri)
```

L'output sarà `5`, poiché l'elenco contiene 5 elementi.

Infine, è importante notare che `len` può anche essere utilizzato come un argomento per altri comandi, come `for` o `while`.

## Vedi anche

- Documentazione ufficiale di Fish Shell: https://fishshell.com/docs/current/
- Guida introduttiva a Fish Shell: https://fishshell.com/docs/current/tutorial.html
- Altri comandi utili in Fish Shell: https://fishshell.com/docs/current/commands.html

In questo articolo abbiamo imparato come utilizzare Fish Shell per ottenere la lunghezza di una stringa, così come altre informazioni utili sul comando `len`. Spero che tu possa utilizzare queste conoscenze nei tuoi progetti futuri. Buon coding!