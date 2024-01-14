---
title:                "Fish Shell: Confronto tra due date"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Confrontare due date può essere una necessità comune quando si programma in Fish Shell. Ad esempio, si potrebbe voler verificare se una determinata data è successiva a un'altra, o se sono identiche. In questo articolo, impareremo come confrontare due date in modo efficiente utilizzando il Fish Shell.

## Come fare

Per confrontare due date in Fish Shell, possiamo utilizzare la funzione integrata `and` seguita dai parametri `I` e `I`. Questi parametri rappresentano rispettivamente la prima e la seconda data da confrontare. Ad esempio, se vogliamo confrontare le date ''15 settembre 2021'' e ''20 settembre 2021'', il codice sarebbe il seguente:

```
and (date -f %d-%m-%Y 15-09-2021) (date -f %d-%m-%Y 20-09-2021)

```

Quando eseguiamo questo comando, otteniamo un valore booleano come risultato. Se la prima data è precedente alla seconda, il risultato sarà `true`, altrimenti sarà `false`.

Possiamo anche utilizzare i parametri `I` e `I` in combinazione con altre funzioni per eseguire confronti più specifici. Ad esempio, possiamo utilizzare la funzione integrata `>`, `<` o `=` per verificare se una data è successiva, precedente o uguale a un'altra. Ecco un esempio di codice che utilizza questi operatori:

```
# Verifica se la prima data è successiva alla seconda
and (date -f %d-%m-%Y 20-09-2021) > (date -f %d-%m-%Y 15-09-2021)

# Verifica se la prima data è precedente alla seconda
and (date -f %d-%m-%Y 15-09-2021) < (date -f %d-%m-%Y 20-09-2021)

# Verifica se le due date sono identiche
and (date -f %d-%m-%Y 20-09-2021) = (date -f %d-%m-%Y 20-09-2021)
```

## Approfondimento

Il Fish Shell offre molte altre funzioni e opzioni per confrontare date in modo più approfondito. Per ulteriori informazioni, si consiglia di consultare la documentazione ufficiale di Fish Shell o la pagina manuale del comando `date`.

## Vedi anche

- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/)
- [Pagina manuale del comando `date`](https://fishshell.com/docs/current/index.html#date)