---
title:                "PHP: Trovare la lunghezza di una stringa"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Perché
Da programmatore, ci troviamo spesso a dover manipolare le stringhe all'interno dei nostri codici. Una delle operazioni più comuni è quella di trovare la lunghezza di una stringa. Scopriamo insieme perché questa è una skill indispensabile nella programmazione PHP.

# Come fare
```PHP
$stringa = "Ciao mondo";
$lunghezza = strlen($stringa);
echo "La lunghezza della stringa è ".$lunghezza; // output: La lunghezza della stringa è 10
```

Per trovare la lunghezza di una stringa in PHP, dobbiamo utilizzare la funzione predefinita `strlen()`. Essa accetta come parametro la stringa di cui vogliamo conoscere la lunghezza e restituisce il numero di caratteri. In questo esempio, abbiamo una stringa di 10 caratteri e il risultato dell'operazione è correttamente 10. 

Nel caso in cui la nostra stringa contenga degli spazi vuoti o dei caratteri speciali, la funzione restituirà comunque il numero totale di caratteri presenti.

# Deep Dive
La funzione `strlen()` basa il suo conteggio sulla codifica dei caratteri utilizzata nel nostro documento PHP. Se stiamo lavorando con una codifica UTF-8, ad esempio, alcuni caratteri speciali potrebbero essere codificati su più byte. Ciò potrebbe portare a un conteggio errato della lunghezza della stringa. 

In questi casi, la soluzione più semplice è utilizzare la funzione `mb_strlen()`, che tiene conto della codifica dei caratteri e restituisce il numero corretto di caratteri anche per le stringhe multibyte.

Un'altro aspetto da considerare è la differenza tra la lunghezza di una stringa e il numero di caratteri presenti in essa. Ad esempio, nella stringa "Ciao" abbiamo 4 caratteri ma la sua lunghezza è comunque 4 perché gli spazi vuoti non vengono conteggiati. Tuttavia, se vogliamo includere anche gli spazi vuoti nella nostra conta, possiamo utilizzare la funzione `str_word_count()` che restituisce il numero totale di parole nella stringa.

# Vedi anche
- [Documentazione ufficiale di PHP su strlen()](https://www.php.net/manual/en/function.strlen.php)
- [Documentazione ufficiale di PHP su mb_strlen()](https://www.php.net/manual/en/function.mb-strlen.php)
- [Documentazione ufficiale di PHP su str_word_count()](https://www.php.net/manual/en/function.str-word-count.php)