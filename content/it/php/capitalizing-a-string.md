---
title:    "PHP: Maiuscolare una stringa"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Ci sono diverse ragioni per cui potresti voler capitalizzare una stringa in PHP. Potresti avere un input utente che necessita di essere uniformato per motivi di formattazione o potresti avere un file di dati che richiede una formattazione specifica per essere elaborato correttamente.

## Come

Per capitalizzare una stringa in PHP, puoi utilizzare la funzione `ucwords()`. Questa funzione prende in input una stringa e restituisce la stessa stringa con la prima lettera di ogni parola in maiuscolo.

```PHP
<?php
$stringa = "ciao a tutti";
echo ucwords($stringa); //stamperà "Ciao A Tutti"
?>
```

Puoi anche utilizzare la funzione `strtoupper()` per rendere tutte le lettere di una stringa maiuscole.

```PHP
<?php
$stringa = "benvenuti nel mondo del PHP";
echo strtoupper($stringa); //stamperà "BENVENUTI NEL MONDO DEL PHP"
?>
```

## Deep Dive

Se vuoi capitalizzare solo la prima lettera di una stringa senza alterare il resto delle lettere, puoi utilizzare la funzione `ucfirst()`. Allo stesso modo, se vuoi rendere tutte le lettere minuscole, puoi utilizzare la funzione `strtolower()`.

È importante notare che queste funzioni funzionano solo per le lingue che utilizzano l'alfabeto latino. Per altre lingue, potresti dover utilizzare funzioni di conversione specifiche.

## Vedi Anche

- [Documentazione PHP su ucwords()](https://www.php.net/manual/en/function.ucwords.php)
- [Documentazione PHP su strtoupper()](https://www.php.net/manual/en/function.strtoupper.php)
- [Documentazione PHP su ucfirst()](https://www.php.net/manual/en/function.ucfirst.php)
- [Documentazione PHP su strtolower()](https://www.php.net/manual/en/function.strtolower.php)