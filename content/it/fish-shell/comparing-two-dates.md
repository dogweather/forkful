---
title:                "Confrontare due date"
html_title:           "Fish Shell: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Hai mai avuto bisogno di confrontare due date in un programma? Forse stavi cercando di verificare se un evento è avvenuto prima di un altro, o forse stai cercando di confrontare le date di nascita dei tuoi amici. In ogni caso, la comparazione di date è un'operazione comune nella programmazione e la buona notizia è che il Fish Shell ha una funzione integrata per aiutarti a farlo.

## Come fare

Per confrontare due date nel Fish Shell, dovrai utilizzare il comando `date` seguito dall'operatore `-lt` (less than) o `-gt` (greater than), a seconda di quale tipo di comparazione stai cercando di fare. Ecco un semplice esempio:

```
Fish Shell >>> date 2021-01-01 -lt 2021-01-02
true
```

Il comando sopra confronta le date 1 gennaio 2021 e 2 gennaio 2021 e restituisce il valore booleano `true` perché la prima data è effettivamente più piccola della seconda. Se il risultato del confronto deve essere assegnato a una variabile, puoi farlo come segue:

```
Fish Shell >>> set prima_data 2021-01-01
Fish Shell >>> set seconda_data 2021-01-02
Fish Shell >>> set confronto (date $prima_data -lt $seconda_data)
```

## Approfondimento

Ma cosa succede se vuoi confrontare date che non segono il formato standard `YYYY-MM-DD`? In questo caso, puoi utilizzare il comando `date` con l'opzione `-f` seguita da un pattern che descrive come la data è strutturata. Ad esempio, se le tue date sono nel formato `YYYY/MM/DD`, puoi procedere come segue:

```
Fish Shell >>> date -f "%Y/%m/%d" 2021/01/01 -lt 2021/01/02
true
```

Inoltre, è possibile utilizzare il comando `date` per confrontare anche le ore. Ad esempio, se vuoi verificare se un evento è avvenuto prima di un certo orario, puoi specificarlo come parte della data da confrontare:

```
Fish Shell >>> date -f "%H:%M" 11:30 -lt 12:00
true
```

Per ulteriori informazioni su come utilizzare il comando `date` e le opzioni disponibili, puoi sempre consultare la documentazione ufficiale.

## Vedi anche

- [Documentazione ufficiale sulla funzione date del Fish Shell](https://fishshell.com/docs/current/commands.html#date)
- [Tutorial su come usare la funzione date nel Fish Shell](https://thoughtbot.com/blog/fish-shell-date)