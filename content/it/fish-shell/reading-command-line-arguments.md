---
title:    "Fish Shell: Lettura degli argomenti della riga di comando"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Perché

Molti programmatori potrebbero chiedersi perché dovrebbero imparare a leggere gli argomenti della riga di comando. La verità è che utilizzare la riga di comando è fondamentale per l'efficienza dei processi di programmazione e può aiutare a creare script e programmi più potenti e flessibili.

## Come fare

Ci sono diverse opzioni per leggere gli argomenti della riga di comando utilizzando il Fish Shell. Vediamo alcuni esempi di codice e le relative output utilizzando i blocchi di codice "```Fish Shell ... ```".

```Fish Shell
set options (getopt -l --long somma,"S=some-option"
)
for i in $options
    switch $i
    case '--somma' switch $next
        case "1" echo "E' stato inserito il numero 1"
        case "2" echo "E' stato inserito il numero 2"
        case "*"
            echo "Errore: L'opzione somma richiede uno dei seguenti argomenti: 1, 2"
            exit 1
        end
end
```

Output:
```
E' stato inserito il numero 1
```

Questo esempio utilizza la funzione `getopt` per leggere gli argomenti della riga di comando e la condizione `switch` per gestire le diverse possibili opzioni.

```Fish Shell
set option (argparse --description "Questo è un esempio di lettura di argomenti" -r somma:"Inserisci un numero:"  sum)
switch $option
    case "sum" echo "La somma è $sum"
    case "*" echo "Errore: Non è stato inserito il valore della somma"
end
```

Output:
```
La somma è 10
```

In questo secondo esempio, viene utilizzata la funzione `argparse` per definire l'opzione `somma` e il valore che deve essere inserito dall'utente. Il valore viene poi ottenuto utilizzando la variabile `$sum` all'interno della condizione `switch`.

## Approfondimenti

Oltre agli esempi forniti, ci sono altri metodi e funzioni che possono essere utilizzati per leggere gli argomenti della riga di comando utilizzando il Fish Shell. Alcune di queste sono:

- `contains`: funzione che permette di verificare se una variabile contiene un valore specifico
- `$*`: variabile che contiene tutti gli argomenti passati dalla riga di comando
- `status`: variabile che contiene lo status code ritornato dal comando precedente

Per ulteriori informazioni e dettagli su come utilizzare queste e altre funzioni per leggere gli argomenti della riga di comando, consiglio di consultare la documentazione ufficiale del Fish Shell.

## Vedi anche

- [Documentazione ufficiale del Fish Shell](https://fishshell.com/docs/current/