---
title:                "Fish Shell: Ottenere la data corrente"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Capita spesso di dover utilizzare la data corrente nei nostri programmi, ad esempio per stamparla su schermo o effettuare calcoli basati sul giorno o mese corrente. Imparare come ottenere automaticamente la data corrente può semplificare la nostra vita da programmatori.

## Come Fare

Il comando `date` ci permette di ottenere la data corrente nel formato standard ISO. Ecco un esempio di come visualizzarla su schermo utilizzando Fish Shell:

```Fish Shell
date
```

Output: `2019-07-10`

Se invece vogliamo avere la data nel formato giorno/mese/anno, possiamo utilizzare la seguente sintassi:

```Fish Shell
date +%d/%m/%Y
```

Output: `10/07/2019`

Possiamo anche specificare una data diversa da quella corrente, utilizzando il formato numerico `YYYY/MM/DD`. Ad esempio:

```Fish Shell
date -d 1996/02/14 +%A
```

Output: `giovedì`

Possiamo anche effettuare calcoli sulla data corrente, ad esempio per ottenere la data di ieri o quella di domani:

```Fish Shell
date -d "yesterday" +%d/%m/%Y
```

Output: `09/07/2019`

```Fish Shell
date -d "+1 day" +%d/%m/%Y
```

Output: `11/07/2019`

## Approfondimento

Il comando `date` è parte del coreutils package e accetta molte altre opzioni per ottenere la data in diversi formati e eseguire calcoli su di essa. Per ulteriori informazioni, è possibile consultarne la pagina di manuale con il comando `man date`.

## Vedi Anche

- [Manuale di Fish Shell](https://fishshell.com/docs/current/index.html)
- [Pagina di manuale di date](https://man7.org/linux/man-pages/man1/date.1.html)
- [Altro articolo su Fish Shell (in italiano)](https://fulgid.eu/post/introduzione-a-fish-shell)