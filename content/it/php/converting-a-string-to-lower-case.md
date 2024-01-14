---
title:    "PHP: Convertire una stringa in minuscolo"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui uno sviluppatore potrebbe voler convertire una stringa in minuscolo. Può essere utile per la normalizzazione dei dati o per confrontare stringhe senza dover preoccuparsi delle maiuscole e minuscole.

## Come fare
Ecco un esempio di come convertire una stringa in minuscolo in PHP utilizzando la funzione `strtolower()`:

```PHP 
<?php
$stringa = "Questa è una Stringa di Esempio";
echo strtolower($stringa);
```

L'output di questo esempio sarebbe: "questa è una stringa di esempio".

Inoltre, è possibile utilizzare la funzione `mb_strtolower()` per supportare più di una lingua.

```PHP
<?php
$stringa = "läsa detta på svenska";
echo mb_strtolower($stringa, 'UTF-8');
```

L'output di questo esempio sarebbe: "läsa detta på svenska".

## Approfondimento
La funzione `strtolower()` utilizza le regole di localizzazione del sistema per convertire i caratteri. Ciò significa che se il sistema utilizza un set di caratteri diverso, la funzione potrebbe non funzionare correttamente. Inoltre, le lingue che utilizzano caratteri diversi dall'alfabeto latino richiedono l'utilizzo della funzione `mb_strtolower()` per una conversione corretta.

## Vedi anche
- [Funzione strtolower](https://www.php.net/manual/en/function.strtolower.php)
- [Funzione mb_strtolower](https://www.php.net/manual/en/function.mb-strtolower.php)