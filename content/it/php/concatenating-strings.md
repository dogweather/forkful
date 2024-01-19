---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Che cos'è e perchè?

La concatenazione di stringhe è l'operazione di unire due o più stringhe per formarne una nuova. I programmatori lo fanno per creare dinamicamente contenuti di testo, come messaggi personalizzati per gli utenti.

## Come fare:

Nella programmazione PHP, usiamo l'operatore di concatenazione '.' per unire le stringhe. Guarda l'esempio di seguito:

```PHP
$primaStringa = "Ciao, ";
$secondaStringa = "mondo!";
$fraseCompleta = $primaStringa . $secondaStringa;

echo $fraseCompleta;
```

Otterrai come output:

```PHP
"Ciao, mondo!"
```

## Approfondendo:

La concatenazione delle stringhe è un concetto fondamentale della programmazione, presente fin dai primi linguaggi come COBOL e FORTRAN. Nelle versioni precedenti di PHP, l'operatore di concatenazione poteva essere un po' confuso perché usava gli stessi simboli ('+' o '.') usati per l'aggiunta matematica. A partire da PHP 4, l'operatore per la concatenazione è diventato '.'.

Un'alternativa alla concatenazione utilizza le virgolette doppie ("") che permettono di includere variabili direttamente all'interno della stringa: 

```PHP
$nome = "Mondo";
echo "Ciao, $nome!";
```

Rispetto alla concatenazione di stringhe, questo approccio può essere più leggibile, ma nota che non può essere utilizzato con le virgolette singole ('').

## Vedi anche:

- [PHP Manual: Concatenazione di stringhe](http://php.net/manual/en/language.operators.string.php)
- [PHP: The Right Way - Concatenazione di stringhe](http://www.phptherightway.com/pages/The-Basics.html#string-concatenation)
- [Bello e semplice: Un'introduzione alla stringa in PHP](https://developer.mozilla.org/it/docs/Learn/PHP/Stringhe)
  
Ricorda, la pratica rende perfetti. Quindi continua a codificare!