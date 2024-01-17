---
title:                "Ottenere la data corrente"
html_title:           "Fish Shell: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Ottenere la data corrente è un'operazione comune tra i programmatori. In sostanza, si tratta di recuperare la data attuale nel formato desiderato per utilizzarla in un programma o script. 

I programmatori spesso utilizzano la data corrente per una varietà di scopi, come ad esempio per tenere traccia del tempo di esecuzione di uno script o per etichettare i file con la data di creazione. Con Fish Shell, è possibile ottenere la data corrente in modo rapido e semplice. Vediamo come.

## Come:

```Fish Shell
$ date
gio 29 lug 2021 20:53:32 CEST
```

Il comando da utilizzare per ottenere la data corrente in Fish Shell è semplicemente `date`. Questo restituirà la data attuale nel formato di default, che potrebbe variare a seconda della configurazione del tuo sistema.

Per ottenere la data nel formato specifico che desideri, puoi utilizzare le opzioni del comando `date`. Ad esempio, se vuoi ottenere la data in formato ISO 8601 (AAAAMMGG), puoi utilizzare il parametro `-I` come segue: 

```Fish Shell
$ date -I
20210729
```

Se vuoi aggiungere anche l'ora al formato ISO, puoi utilizzare il parametro `-I seconds`: 

```Fish Shell
$ date -I seconds
20210729T205332+0200
```

Per ulteriori opzioni disponibili con il comando `date`, puoi consultare la sua pagina di manuale utilizzando il comando `man date`.

## Deep Dive:

Il comando `date` è una utility di sistema comune utilizzata per visualizzare o impostare la data e l'ora del sistema. È presente in molti sistemi operativi, tra cui Linux, macOS e altri sistemi Unix-like.

Nella maggior parte dei sistemi, `date` utilizza l'orologio di sistema (RTC - Real Time Clock) per ottenere la data e l'ora correnti. Questo significa che se la data o l'ora sono configurate in modo errato nel sistema, il comando `date` restituirà valori errati.

Oltre al comando `date`, ci sono anche altri modi per ottenere la data corrente in Fish Shell. Ad esempio, puoi utilizzare la variabile d'ambiente `__fish_date` per ottenere la data nel formato `%Y%m%d` (anno, mese, giorno):

```Fish Shell
$ echo $__fish_date
20210729
```

Oppure, puoi utilizzare il comando `strftime`, che formatta una data e un'ora specificata in base a una stringa di formato. Ad esempio, per ottenere la data corrente nel formato `%Y-%m-%d` (anno-mese-giorno), puoi utilizzare il seguente comando:

```Fish Shell
$ strftime '%Y-%m-%d' now
2021-07-29
```

## See Also:

- [Documentazione ufficiale Fish Shell](https://fishshell.com/docs/current/index.html)
- [Pagina di manuale del comando date](https://fishshell.com/docs/current/cmds/date.html)
- [Info su come utilizzare le variabili d'ambiente in Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_set) 
- [Pagina di manuale del comando strftime](https://fishshell.com/docs/current/cmds/strftime.html)