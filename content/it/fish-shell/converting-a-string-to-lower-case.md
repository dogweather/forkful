---
title:    "Fish Shell: Convertire una stringa in minuscolo"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Perché

Convertingire una stringa in minuscolo è un'operazione fondamentale nella programmazione. Spesso, è necessario manipolare le stringhe di testo per ottenere una formattazione uniforme o per confrontare diverse parole indipendentemente dalle maiuscole o minuscole. Imparare a convertire le stringhe in minuscolo può semplificare il tuo lavoro e renderlo più efficiente.

## Come Fare

Per convertire una stringa in minuscolo utilizzando Fish Shell, è possibile utilizzare il comando `string tolower`. Ad esempio:

```Fish Shell
$string = "Ciao a Tutti"
echo (string tolower $string)
```

L'output sarà `ciao a tutti`, con tutte le lettere convertite in minuscolo.

Puoi anche combinare il comando `string tolower` con altri comandi per ottenere una formattazione specifica. Ad esempio, puoi utilizzare il comando `string sub` per sostituire parti di una stringa con il testo in minuscolo. Ecco un esempio:

```Fish Shell
$string = "Benvenuto nel MONDO della Programmazione!"
echo (string sub --lower (string tolower $string) "mondo" "universo")
```

L'output sarà `benvenuto nel universo della programmazione!`, con la parola "mondo" sostituita con "universo" e tutto il testo convertito in minuscolo.

## Approfondiamo

Esistono diverse funzioni all'interno di Fish Shell che possono essere utilizzate per convertire una stringa in minuscolo. Il comando `string tolower` accetta opzioni aggiuntive, come `--all`, che convertirà tutte le lettere in minuscolo, comprese le lettere accentate che di solito non verrebbero convertite.

Inoltre, se stai lavorando con i caratteri Unicode, puoi utilizzare il comando `string tolower --length` per specificare il numero di caratteri che vuoi convertire in minuscolo. Ad esempio, `string tolower --length=4` convertirà solo i primi 4 caratteri nella stringa.

Inoltre, se hai bisogno di una conversione bidirezionale, cioè convertire sia le lettere minuscole che quelle maiuscole in minuscolo, puoi utilizzare il comando `string swapcase` in combinazione con `string tolower` per ottenere questo risultato.

## Vedi Anche

- [Fish Shell documentazione ufficiale](https://fishshell.com/docs/current/)
- [Tutorial su Fish Shell per principianti](https://dev.to/gaelthomas/fish-shell-tutorial-for-beginners-30i5)
- [10 comandi utili di Fish Shell che dovresti conoscere](https://medium.com/pragmatic-programmers/10-useful-fish-shell-commands-you-should-know-ab5142b25611)