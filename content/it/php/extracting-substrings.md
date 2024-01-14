---
title:                "PHP: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

L'estrazione di sottostringhe è un'operazione fondamentale nella programmazione PHP. Permette di ottenere una parte specifica di una stringa più grande e può essere utile in molte situazioni, come ad esempio l'analisi delle informazioni di un indirizzo email o la gestione dei dati inseriti dagli utenti.

## Come fare

Per estrarre una sottostringa in PHP, è possibile utilizzare la funzione `substr()`. Questa funzione richiede almeno due parametri: la stringa di origine e la posizione di inizio della sottostringa desiderata. Inoltre, è possibile specificare un terzo parametro per definire la lunghezza della sottostringa, altrimenti verrà estratta tutta la parte rimanente della stringa di origine.

```PHP
$stringa = "Ciao, mi chiamo Marco!";
$sottostringa = substr($stringa, 6); // $sottostringa = "mi chiamo Marco!"
$sottostringa = substr($stringa, 6, 8); // $sottostringa = "mi chiamo"
```

Se il parametro di lunghezza è un numero negativo, la sottostringa verrà estratta a partire dalla fine della stringa di origine.

```PHP
$stringa = "Ciao, mi chiamo Marco!";
$sottostringa = substr($stringa, -6); // $sottostringa = "Marco!"
```

Inoltre, è possibile specificare una stringa di fine personalizzata usando l'argomento opzionale `end` della funzione `mb_strstr()`, come mostrato di seguito:

```php
$stringa = "Ciao, mi chiamo Marco!";
//$sottostringa = "mi chiamo" (come sopra)
$sottostringa = mb_stristr($stringa, "chiamo", true); 
```

## Approfondimento

La funzione `substr()` è molto utile per estrarre una parte di una stringa, ma ci sono alcune precauzioni da prendere in considerazione. Ad esempio, se si utilizzano caratteri multi-byte come nell'esempio precedente, è importante utilizzare le funzioni `mb_substr()` e `mb_stristr()` per assicurarsi che la sottostringa venga estratta correttamente anche con caratteri speciali.

Un'altra cosa da tenere presente è che la funzione `substr()` non è in grado di gestire stringhe unicode, quindi in questi casi è necessario utilizzare funzioni più avanzate come `mb_substr()` o `grapheme_substr()`.

## Vedi anche

- [Funzione substr() su PHP.net](https://www.php.net/manual/it/function.substr.php)
- [Funzioni multibyte su PHP.net](https://www.php.net/manual/it/book.mbstring.php)