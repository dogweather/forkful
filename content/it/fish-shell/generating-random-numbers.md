---
title:                "Generazione di numeri casuali"
html_title:           "Fish Shell: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Quando si scrive un codice, può essere utile generare numeri casuali per simulare situazioni o per testare determinate funzioni. Il modo più semplice e veloce per farlo in Fish Shell è utilizzando il comando `fish_random`.

## Come Fare

Per generare un numero casuale tra 0 e 10, basta usare il seguente codice:

```Fish Shell
set random_number (fish_random 0 10)
echo $random_number
```

Il risultato sarà un numero casuale tra 0 e 10, ad esempio `7`.

Se si vuole generare più di un numero casuale, ad esempio una lista di 10 numeri tra 1 e 100, si può utilizzare un ciclo for e il comando `math` per generare numeri casuali:

```Fish Shell
for i in (seq 1 10)
    set random_number (math "(fish_random)" * 100 + 1)
    echo $random_number
end
```

Il comando `math` consente di fare calcoli matematici sul numero generato da `fish_random`, moltiplicando per 100 e aggiungendo 1 per ottenere numeri tra 1 e 100. L'output del codice sopra sarà una lista di 10 numeri casuali, ad esempio `42, 3, 78, 91, 17, 10, 64, 54, 30, 97`.

## Approfondimento

Il comando `fish_random` è basato sull'algoritmo di generazione di numeri casuali di Park-Miller, che è stato dimostrato essere abbastanza efficiente e di buona qualità. Tuttavia, è importante ricordare che i numeri generati non saranno veramente casuali, ma seguono uno schema prevedibile definito dall'algoritmo. Quindi, se si hanno esigenze di sicurezza o si richiede una vera casualità, è meglio utilizzare altri metodi di generazione di numeri come l'uso di hardware specializzato o API di servizi di terze parti.

## Vedi Anche

- Documentazione ufficiale di Fish Shell su `fish_random`: https://fishshell.com/docs/current/cmds/random.html
- Articolo su Park-Miller Random Number Generator: https://en.wikipedia.org/wiki/Lehmer_random_number_generator#Park%E2%80%93Miller_generator