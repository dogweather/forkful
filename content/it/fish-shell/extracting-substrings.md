---
title:    "Fish Shell: Estrazione di sottostringhe"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

L'estrazione delle sottostringhe è un'operazione utile nella programmazione in Fish Shell perché consente di lavorare con parti specifiche di una stringa, anziché con l'intera stringa.

## Come fare

L'estrazione delle sottostringhe in Fish Shell è possibile grazie al comando `string sub`. Utilizzando questo comando, è possibile specificare l'indice di inizio e l'indice di fine della sottostringa desiderata. Ecco un esempio di codice:

```
Fish Shell
string sub "ciao mondo" 2 6
```

E questo è l'output che otterremo:

```
iao m
```

In questo caso, abbiamo estratto la sottostringa "iao m" dalla stringa "ciao mondo", partendo dall'indice 2 (incluso) e finendo all'indice 6 (escluso).

## Approfondimento

L'indice di una stringa in Fish Shell inizia sempre da 1, quindi quando si estraggono le sottostringhe, è importante tenere conto di questo dettaglio. Inoltre, è possibile utilizzare anche un indice negativo per indicare l'ultimo carattere della stringa. Ad esempio:

```
Fish Shell
string sub "ciao mondo" 2 -1
```

Restituirà la sottostringa "iao mondo".

Un'altra opzione utile è quella di utilizzare il comando `string length` per ottenere la lunghezza totale della stringa, in modo da poter estrarre una sottostringa senza dover conoscere l'indice esatto dei caratteri desiderati.

## Vedi anche

- Documentazione ufficiale di Fish Shell su `string sub`: https://fishshell.com/docs/current/cmds/string_sub.html
- Altri comandi utili per la manipolazione di stringhe: https://fishshell.com/docs/current/cmds.html#string-manipulation