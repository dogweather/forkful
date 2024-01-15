---
title:                "Utilizzando le espressioni regolari"
html_title:           "Fish Shell: Utilizzando le espressioni regolari"
simple_title:         "Utilizzando le espressioni regolari"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Se sei un utente di Fish Shell, potresti chiederti perché dovresti imparare ad utilizzare le espressioni regolari. La risposta è semplice: le espressioni regolari sono uno strumento potente per la ricerca e la manipolazione di testo all'interno della shell. Con una conoscenza delle espressioni regolari, puoi utilizzare il tuo terminale in modo più efficiente e produttivo.

## Come si usano le espressioni regolari in Fish Shell

Le espressioni regolari sono supportate in Fish Shell utilizzando il comando `string match` e l'operatore `=~`. Per esempio, se vogliamo trovare tutti i file di testo nella cartella corrente, possiamo utilizzare la seguente espressione regolare all'interno di un ciclo `for`:

```Fish Shell
for file in *
    string match -r '*.txt' $file
    if test $status -eq 0
        echo $file
    end
end
```

Questo codice utilizza l'opzione `-r` per indicare che stiamo utilizzando un'espressione regolare. La stringa `*.txt` indica che vogliamo trovare tutti i file con estensione `.txt`. Se l'espressione regolare corrisponde al nome del file, il comando `string match` restituirà un codice di uscita 0, altrimenti restituirà un codice di uscita diverso da 0. Utilizzando l'operatore `test` possiamo verificare il codice di uscita del comando e procedere di conseguenza.

## Approfondimento

Se vuoi saperne di più sulle espressioni regolari, puoi consultare la documentazione ufficiale di Fish Shell o fare una ricerca online per trovare tutorial e guide. Una cosa importante da ricordare è che ci sono diverse varianti di sintassi per le espressioni regolari e può essere utile familiarizzare con più di una per essere in grado di utilizzarle in modo flessibile.

## Vedi anche

- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/)
- [Sintassi delle espressioni regolari](https://www.regular-expressions.info/fish.html)
- [Tutorial su espressioni regolari in Fish Shell](https://medium.com/learn-to-code-with-vanilla-javascript/regular-expressions-in-fish-shell-dz-cd0531c8d8d6)