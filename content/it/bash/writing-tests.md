---
title:    "Bash: Scrivere test"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Perché 
Scrivere test è fondamentale per garantire la qualità e la funzionalità del codice. I test ci permettono di individuare e risolvere errori in maniera efficiente, garantendo una maggiore stabilità e affidabilità del nostro programma.

## Come fare 
Per scrivere test in Bash, è necessario utilizzare il comando `assert`. Possiamo usare questo comando per verificare se un'asserzione sia vera o falsa. Se l'asserzione è vera, il programma continuerà a eseguire il codice. Se l'asserzione è falsa, il programma terminerà con un errore. Ecco un esempio di come utilizzare `assert` in uno script Bash: 
```Bash 
#Definiamo una funzione che vogliamo testare 
function somma() { 
   local a=$1 
   local b=$2 
   return $(($a + $b)) 
} 

#Chiamiamo la funzione e confrontiamo il risultato con l'output atteso 
assert "somma 2 3" "5" 
``` 
Nell'esempio sopra, stiamo testando la funzione `somma`, che dovrebbe ritornare la somma dei due parametri passati. Se il risultato della funzione corrisponde a 5, il test passerà con successo. In caso contrario, il test fallirà e saranno mostrati i dettagli dell'errore.

## Approfondimento 
Scrivere test efficaci è un processo che richiede tempo e dedizione. È importante pianificare quali parti del codice testare e scrivere test che coprano tutti gli scenari possibili. Inoltre, è importante essere in grado di interpretare i risultati dei test e di utilizzare gli errori per migliorare il codice. In generale, è sempre meglio scrivere troppi test che troppi pochi. 

## Vedi anche 
- [Documentazione di `assert` di Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-assert)
- [Guida su come scrivere test efficaci in Bash](https://david.rothlis.net/bash/how-to-write-tests.html)
- [Esempi di test in Bash](https://github.com/sstephenson/bats/tree/master/examples)