---
title:                "PHP: Generazione di numeri casuali"
programming_language: "PHP"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

I numeri casuali sono un elemento fondamentale nella programmazione. Possono essere utilizzati per creare giochi, generare password sicure e molto altro. Generare numeri casuali in PHP può aumentare l'interattività e l'interesse dei tuoi progetti.

## Come fare

Per generare numeri casuali in PHP, puoi utilizzare la funzione `rand()`. Questa funzione richiede due parametri: il valore minimo e il valore massimo. Utilizzando l'operatore `echo`, possiamo visualizzare il numero casuale generato.

```PHP
echo rand(1, 100); // Visualizza un numero casuale tra 1 e 100
```

Puoi anche utilizzare `mt_rand()` per generare numeri casuali ancora più sicuri e casuali. Questa funzione richiede anche due parametri, ma restituisce un numero maggiore di possibili risultati.

```PHP
echo mt_rand(1, 100); // Visualizza un numero casuale tra 1 e 100
```

Puoi anche utilizzare la funzione `rand()` per generare lettere casuali. Ad esempio, per generare una lettera casuale tra A e Z, possiamo utilizzare il codice seguente:

```PHP
$lettera = chr(rand(65, 90));
echo $lettera; // Visualizza una lettera casuale tra A e Z
```

## Approfondimento

La generazione di numeri casuali in PHP non è completamente casuale, ma utilizza un algoritmo per generare numeri pseudo-casuali. Ciò significa che i numeri sono determinati da uno specifico algoritmo piuttosto che da vera casualità. Tuttavia, l'utilizzo di funzioni come `rand()` e `mt_rand()` garantisce una buona casualità per le tue esigenze di programmazione.

Se desideri una maggiore casualità, puoi anche utilizzare fonti esterne come la temperatura o la posizione GPS per influenzare la generazione dei numeri casuali.

## Vedi anche

- [Documentazione ufficiale di PHP sulla funzione rand()](https://www.php.net/manual/en/function.rand.php)
- [Documentazione ufficiale di PHP sulla funzione mt_rand()](https://www.php.net/manual/en/function.mt-rand.php)
- [Generare numeri casuali in PHP: una guida pratica](https://www.digitalocean.com/community/tutorials/how-to-generate-random-numbers-in-php)