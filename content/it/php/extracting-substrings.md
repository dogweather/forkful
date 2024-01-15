---
title:                "Estrazione di sottostringhe"
html_title:           "PHP: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Estrarre delle sottostringhe può essere molto utile quando si sta manipolando del testo. Ad esempio, potresti voler ottenere una parte specifica di una stringa, come il cognome da un nome e cognome intero. Oppure, potresti dover verificare se una certa parola è presente in una stringa più grande.

## Come Fare

Estrarre delle sottostringhe in PHP è molto semplice e intuitivo. Utilizzando la funzione `substr` è possibile specificare da quale carattere iniziare e quanti caratteri includere nella sottostringa. Ad esempio:

```
<?php
$stringa = "Ciao a tutti!";
$sottostringa = substr($stringa, 5, 2);
echo $sottostringa; // output: a t
?>
```

In questo esempio, la funzione `substr` parte dal sesto carattere della stringa (contando anche gli spazi) e include i successivi due caratteri.

Se si vuole ottenere una sottostringa a partire dall'ultima occorrenza di un carattere, si può utilizzare la funzione `strrpos` per trovare la posizione di quel carattere all'interno della stringa. Ad esempio:

```
<?php
$stringa = "ciao a tutti!";
$sottostringa = substr($stringa, strrpos($stringa, " ") + 1);
echo $sottostringa; // output: tutti!
?>
```

In questo caso, la funzione `strrpos` viene utilizzata per trovare la posizione dello spazio all'interno della stringa e, aggiungendo 1, si ottiene la posizione del primo carattere della parola successiva.

## Approfondimento

La funzione `substr` è solo una delle numerose funzioni di manipolazione delle stringhe disponibili in PHP. Altre funzioni utili per estrarre delle sottostringhe sono `strpos` per trovare la posizione di un carattere o di una parola all'interno di una stringa, e `explode` per dividere una stringa in un array in base a un delimitatore.

Per ulteriori informazioni su queste funzioni e per saperne di più sulla manipolazione delle stringhe in PHP, si consiglia di consultare la documentazione ufficiale di PHP.

## Vedi Anche

- [Documentazione ufficiale di PHP su substr](https://www.php.net/manual/en/function.substr.php)
- [Funzioni di manipolazione delle stringhe in PHP](https://www.php.net/manual/en/ref.strings.php)