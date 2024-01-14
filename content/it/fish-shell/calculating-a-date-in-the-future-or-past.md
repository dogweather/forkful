---
title:                "Fish Shell: Calcolare una data nel futuro o nel passato"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Molti di noi si trovano spesso a dover calcolare una data futura o passata per vari motivi, come prenotare una vacanza o pianificare un evento. Con questo post, imparerai come farlo facilmente utilizzando il Fish Shell.

## Come Fare

Per prima cosa, apri il Fish Shell e inizia a digitare il seguente comando:

```Fish Shell
date -v +1m
```

Questo comando restituirà la data di un mese da oggi. Puoi utilizzare altri argomenti per cambiare l'intervallo temporale, ad esempio "+5y" per aggiungere 5 anni o "-2d" per sottrarre 2 giorni.

Una volta inseriti gli argomenti desiderati, premi Invio e vedrai subito la nuova data calcolata. Inoltre, puoi combinare più argomenti per ottenere date più precise, ad esempio "+1m +10d" per aggiungere un mese e 10 giorni.

Oltre ad aggiungere o sottrarre giorni, mesi o anni, puoi anche specificare una data di partenza diversa da quella corrente utilizzando il formato "YYYY-MM-DD". Ad esempio, se vuoi calcolare una data futura partendo dal 2020-06-01, il comando completo diventa:

```Fish Shell
date -v +1m +10d 2020-06-01
```

## Approfondimento

Il Fish Shell utilizza il comando date per calcolare le date in modo semplice e intuitivo grazie all'opzione -v che consente di specificare l'intervallo di tempo desiderato. Inoltre, puoi aggiungere il parametro -f per utilizzare un formato di data diverso da quello predefinito.

Ad esempio, se vuoi ottenere la data nel formato "DD/MM/YYYY", puoi utilizzare il seguente comando:

```Fish Shell
date -v +1m -f "%d/%m/%Y"
```

Inoltre, con il Fish Shell puoi anche aggiungere o sottrarre mesi o anni dalle date, utilizzando l'argomento -d seguito da una data specifica nel formato "YYYY-MM-DD". Ad esempio, se vuoi ottenere la data risultante da 6 mesi prima del 2020-06-01, il comando diventa:

```Fish Shell
date -d "2020-06-01 -6m"
```

## Vedi Anche

- [Documentazione Fish Shell](https://fishshell.com/docs/current/cmds/date.html)
- [Esempi di utilizzo del comando date](https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/)
- [Altri trucchi utili del Fish Shell](https://fishshell.com/docs/current/tutorial.html)