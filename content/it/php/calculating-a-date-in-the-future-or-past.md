---
title:    "PHP: Calcolare una data nel futuro o nel passato"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato è un'operazione comune in programmazione, soprattutto quando si lavora con applicazioni che gestiscono eventi o scadenze. Conoscere il modo corretto di calcolare le date può rendere il codice più efficiente e preciso.

## Come eseguirlo

Per calcolare una data nel futuro o nel passato, è necessario utilizzare la funzione `date()` di PHP insieme a un valore di offset in giorni, mesi o anni. Ad esempio, per ottenere la data di oggi più 7 giorni, si può utilizzare il seguente codice:

```PHP
$date = date('Y-m-d', strtotime("+7 days"));
echo $date; // output: 2021-12-07
```

In questo esempio, `strtotime()` converte l'offset specificato in una data valida, che viene quindi formattata con `date()` nel formato desiderato. Inoltre, è possibile combinare più valori di offset per calcolare date più complesse. Ad esempio, per ottenere la data di oggi più 3 mesi e 1 settimana, si può utilizzare il seguente codice:

```PHP
$date = date('Y-m-d', strtotime("+3 months +1 week"));
echo $date; // output: 2022-03-08
```

## Approfondimenti

Il calcolo delle date può rivelarsi più complesso nei casi in cui si devono gestire date che includono anche l'ora o il fuso orario. In questi casi, è consigliabile utilizzare le funzioni specifiche di PHP come `strtotime()` o `DateTime()`, che offrono maggiori opzioni per la gestione delle date e dei tempi.

Inoltre, è importante tenere presente che i valori passati come offset non vengono sempre interpretati nello stesso modo da tutti i server, a causa di possibili differenze nel fuso orario o nella configurazione di PHP. È quindi consigliabile testare sempre il codice su più server per assicurarsi che il risultato sia sempre corretto.

## Vedi anche

- [Documentazione ufficiale di PHP su date()](https://www.php.net/manual/en/function.date.php)
- [Documentazione ufficiale di PHP su strtotime()](https://www.php.net/manual/en/function.strtotime.php)
- [Documentazione ufficiale di PHP su DateTime()](https://www.php.net/manual/en/datetime.php)