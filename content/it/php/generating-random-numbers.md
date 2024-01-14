---
title:    "PHP: Generazione di numeri casuali"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è una pratica comune nei linguaggi di programmazione, poiché può essere utile in molte situazioni diverse. Ad esempio, può essere utilizzata per generare password casuali, per selezionare elementi casuali da un set di dati o per creare simulazioni.

## Come

Per generare numeri casuali in PHP, è possibile utilizzare la funzione `rand()`. Questa funzione richiede due parametri: il primo è il valore minimo che si desidera generare e il secondo è il valore massimo. Ad esempio:

```PHP
$num = rand(1, 10);
echo $num;
```

Questo codice genererà un numero casuale compreso tra 1 e 10 e lo stamperà a schermo.

Puoi anche utilizzare la funzione `mt_rand()`, che utilizza un generatore di numeri pseudo-casuali "più casuale" rispetto alla funzione `rand()`. Esempio:

```PHP
$num = mt_rand(100, 200);
echo $num;
```

## Approfondimento

Ci sono alcune cose importanti da tenere a mente quando si generano numeri casuali in PHP. In primo luogo, questi numeri non sono veramente casuali, ma sono basati su algoritmi matematici che restituiscono un numero casuale "apparente". In secondo luogo, se si desidera generare un numero casuale con una certa precisione, è necessario impostare il secondo parametro della funzione come un valore molto grande, poiché il valore massimo possibile è leggermente inferiore al massimo consentito dal sistema operativo. Infine, se si desidera avere sempre lo stesso numero casuale, è possibile impostare un "seed" con la funzione `srand()` prima di chiamare `rand()` o `mt_rand()`.

## Vedi anche

- [Guida PHP su generazione di numeri casuali](https://www.php.net/manual/en/function.mt-rand.php)
- [Articolo su algoritmi di generazione di numeri casuali](https://www.random.org/randomness/)
- [Tutorial su come generare password sicure con PHP](https://www.w3schools.com/php/func_string_rand.asp)