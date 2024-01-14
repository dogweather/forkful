---
title:    "Fish Shell: Ottenere la data corrente."
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Perché

La data corrente è un'informazione importante per molti programmi e script. Può essere utilizzata per il registro degli eventi, il salvataggio dei file con il nome della data e molto altro ancora.

# Come Fare

Fish Shell è un'ottima scelta per ottenere la data corrente grazie alla sua sintassi semplice e alle sue funzioni integrate. Ecco un esempio di come ottenere la data corrente utilizzando la funzione `date` di Fish Shell:

```Fish Shell
date +%Y-%m-%d
```
Output:
`2021-09-18`

Usando la stringa di formattazione `%Y-%m-%d`, otteniamo la data corrente nel formato anno-mese-giorno. È possibile personalizzare il formato utilizzando diverse stringhe di formattazione, come `%H:%M:%S` per l'ora corrente o `%A` per il nome del giorno della settimana.

Inoltre, Fish Shell supporta anche la funzione `now` che restituisce un oggetto `date` con la data e l'ora correnti. Possiamo quindi utilizzare altre funzioni associate all'oggetto `date`, ad esempio:

```Fish Shell
echo (now +%B)" "(now +%Y)
```
Output:
`settembre 2021`

Questo esempio utilizza la funzione `echo` per stampare il mese e l'anno correnti.

# Approfondimento

Per coloro che vogliono approfondire il funzionamento della data in Fish Shell, c'è una serie di funzioni disponibili. Ad esempio, la funzione `date -r` consente di ottenere la data di ultima modifica di un file, mentre `date --universal` restituisce la data e l'ora universali in UTC.

Inoltre, è possibile eseguire calcoli sulla data utilizzando il timestamp Unix, che rappresenta il numero di secondi trascorsi dal 1° gennaio 1970. Fish Shell offre la funzione `math`, che permette di eseguire operazioni matematiche, come ad esempio:

```Fish Shell
math "(%s - %s) / 60" (date -u +%s) (now -u +%s)
```
Output:
`1`

Questo esempio calcola la differenza in minuti tra la data corrente in UTC e il timestamp Unix della data di oggi in UTC.

# Vedi Anche

- Documentazione ufficiale di Fish Shell su `date`: https://fishshell.com/docs/current/cmds/date.html
- Guida su come formattare la data e l'ora in Fish Shell: https://www.freecodecamp.org/news/how-to-format-date-and-time-in-the-linux-terminal/
- Spiegazione dettagliata sui timestamp Unix: https://en.wikipedia.org/wiki/Unix_time