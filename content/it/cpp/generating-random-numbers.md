---
title:    "C++: Generare numeri casuali"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché
Generare numeri casuali è un'attività molto comune nella programmazione, sia per scopi ludici che per fini più seri, come la creazione di dati di prova o la simulazione di eventi casuali. Inoltre, aiuta a rendere i programmi più dinamici e interessanti per l'utente.

## Come fare
Per generare numeri casuali in C++, è necessario utilizzare la funzione `rand()` dalla libreria standard `cstdlib`. Questa funzione restituisce un numero intero casuale compreso tra 0 e `RAND_MAX`, una costante definita nella libreria. Per ottenere un numero in un determinato intervallo, è possibile utilizzare il seguente codice:

```C++
int random_number = rand() % (max - min + 1) + min;
```

Dove `max` e `min` rappresentano rispettivamente il valore massimo e minimo dell'intervallo desiderato. Ad esempio, per generare un numero casuale compreso tra 1 e 100, si potrebbe utilizzare il codice:

```C++
int random_number = rand() % 100 + 1;
```

È importante inizializzare il generatore di numeri casuali con un seme, altrimenti gli stessi numeri verranno generati ogni volta che il programma viene eseguito. Per fare ciò, è possibile utilizzare il valore del tempo corrente come seme iniziale:

```C++
srand(time(0));
```

Esempio di output:

```
56
12
89
```

## Approfondimento
La funzione `rand()` non è totalmente casuale, poiché utilizza un algoritmo per generare i numeri. Inoltre, il generatore di numeri casuali potrebbe non essere distribuito in modo uniforme, ovvero alcuni numeri possono essere più probabili di altri. Per ovviare a questi problemi, è possibile utilizzare librerie esterne che offrono funzioni di generazione di numeri casuali più sofisticate e accurate, come ad esempio la libreria Boost.Random.

## Vedi anche
- [Documentazione della funzione `rand()`](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Libreria Boost.Random](https://www.boost.org/doc/libs/1_70_0/doc/html/boost_random.html)