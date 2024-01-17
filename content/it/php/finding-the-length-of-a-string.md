---
title:                "Trovare la lunghezza di una stringa"
html_title:           "PHP: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
La lunghezza di una stringa è il numero di caratteri contenuti all'interno di essa. I programmatori spesso devono trovare la lunghezza di una stringa per poter manipolarla o stamparla correttamente.

## Come fare:
Ecco un esempio di codice in PHP per trovare la lunghezza di una stringa e stamparla:
```PHP
$stringa = "Ciao, mondo!";
echo strlen($stringa);
// Output: 12
```
In questo esempio, utilizziamo la funzione `strlen()` per trovare la lunghezza della stringa. Possiamo anche utilizzare un ciclo for per contare manualmente i caratteri all'interno della stringa.

## Approfondimento:
Questa operazione è molto comune nella programmazione, soprattutto quando si lavora con input dell'utente o dati provenienti da fonti esterne. In passato, era comune utilizzare la funzione `strlen()` per trovare la lunghezza di una stringa, ma con l'avvento di PHP7, è stato introdotto il nuovo operatore `??` che può essere utilizzato per contare il numero di caratteri di una stringa.

## Vedi anche:
- Documentazione ufficiale di PHP sulla funzione `strlen()`: https://www.php.net/manual/en/function.strlen.php
- Spiegazione dell'operatore `??` nel blog di PHP: https://www.php.net/manual/en/migration70.new-features.php#migration70.new-features.coalesce