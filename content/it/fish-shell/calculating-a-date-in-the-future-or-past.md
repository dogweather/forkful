---
title:    "Fish Shell: Calcolare una data nel futuro o nel passato"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, nella programmazione, è necessario calcolare una data nel futuro o nel passato per svolgere determinate operazioni. Ad esempio, si potrebbe voler pianificare l'esecuzione di uno script in una data specifica o calcolare il tempo trascorso tra due eventi. La shell del pesce (Fish Shell) offre una semplice e conveniente soluzione per questo tipo di esigenza.

## How To

Il comando `date` di Fish Shell ci permette di calcolare una data nel futuro o nel passato. Vediamo alcuni esempi:

```Fish Shell
# Calcola la data di oggi
echo (date)

# Calcola la data di 7 giorni fa
echo (date -d "7 days ago")

# Calcola la data di 1 mese fa
echo (date -d "1 month ago")
```

Questo comando accetta anche parametri in formato ISO, come ad esempio `yyyy-mm-dd`. Esempio:

```Fish Shell
echo (date -d "2021-12-25")
```

Inoltre, possiamo anche specificare una data come base e calcolare una data future o passata a partire da essa. Esempio:

```Fish Shell
echo (date -d "2021-01-01 + 1 year") # Data del prossimo anno
echo (date -d "2021-01-01 + 2 months") # Data di 2 mesi nel futuro
echo (date -d "2021-01-01 + 1 week") # Data della prossima settimana
```

## Deep Dive

Il comando `date` di Fish Shell utilizza la libreria `libdateutil` per effettuare i calcoli di data. Questa libreria supporta diversi formati di data, tra cui ISO 8601, RFC 2822 e GNU-style relative date.

Inoltre, il comando `date` offre anche la possibilità di calcolare intervalli di tempo, utilizzando l'opzione `-u`. Esempio:

```Fish Shell
echo (date -u +%s)- (date +%s)) # Calcola il numero di secondi trascorsi da UNIX epoch
```

## Vedi anche

- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/index.html)
- [Documentazione ufficiale di libdateutil](https://fishshell.com/docs/current/cmds/date.html)
- [ISO 8601 - Standard internazionale per rappresentare date e orari](https://it.wikipedia.org/wiki/ISO_8601)