---
title:                "PHP: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

In programmazione, è spesso necessario trattare con diverse stringhe di testo. E una delle operazioni più comuni che può essere richiesta è quella di convertire una stringa in caratteri minuscoli. Capirai ora perché sapere come farlo può essere utile per le tue attività di programmazione.

## Come fare

Per convertire una stringa in caratteri minuscoli in PHP, puoi utilizzare la funzione strtolower(). Questa funzione prenderà una stringa come argomento e restituirà la stessa stringa in caratteri minuscoli. Vediamo un esempio:

```PHP
$stringa = "STRINGA IN MAIUSCOLO";
$stringa_minuscola = strtolower($stringa);
echo $stringa_minuscola;
```

L'output sarà:

```
stringa in maiuscolo
```

Ci sono anche altre funzioni che possono essere utilizzate per la conversione di stringhe come strtoupper() per convertire in caratteri maiuscoli e ucfirst() per convertire solo il primo carattere in maiuscolo. È importante notare che queste funzioni non cambieranno la stringa originale ma ne restituiranno una nuova.

## Approfondimento

A livello più avanzato, è importante capire che la conversione in caratteri minuscoli dipende anche dal set di caratteri utilizzato dal sistema operativo e dalle impostazioni del server. Per una maggiore precisione, è consigliabile utilizzare la funzione mb_strtolower() che tiene conto dei set di caratteri multi-byte.

Inoltre, è possibile passare un secondo parametro a strtolower() per specificare il set di caratteri da utilizzare. Questo è particolarmente utile se si desidera convertire una stringa con caratteri speciali o di diverse lingue.

## Vedi anche

- [Funzione strtolower() in PHP](https://www.php.net/manual/en/function.strtolower.php)
- [Funzione strtoupper() in PHP](https://www.php.net/manual/en/function.strtoupper.php)
- [Funzione ucfirst() in PHP](https://www.php.net/manual/en/function.ucfirst.php)
- [Funzione mb_strtolower() in PHP](https://www.php.net/manual/en/function.mb-strtolower.php)