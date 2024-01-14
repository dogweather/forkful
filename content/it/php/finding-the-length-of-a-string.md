---
title:    "PHP: Trova la lunghezza di una stringa"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Scrivere codice PHP può sembrare intimidatorio per molte persone, ma una delle cose più utili che puoi imparare è come trovare la lunghezza di una stringa. Questa abilità ti consentirà di manipolare i dati in modo più efficace e di creare script più robusti.

## Come fare

Per ottenere la lunghezza di una stringa in PHP, è possibile utilizzare la funzione `strlen()`. Questa funzione accetta una stringa come input e restituisce il numero di caratteri all'interno della stringa. Ecco un esempio di codice PHP che utilizza `strlen()`:

```PHP
$stringa = "Ciao mondo!";
echo strlen($stringa); // Output: 11
```

Come puoi vedere, la funzione `strlen()` è semplice da usare e restituisce immediatamente il risultato desiderato. È importante notare che `strlen()` considera anche gli spazi e i caratteri speciali nella stringa.

Se vuoi conoscere la lunghezza di una stringa ma escludere gli spazi, puoi utilizzare la funzione `trim()` prima di `strlen()`, in questo modo:

```PHP
$stringa = "   Ciao mondo!   ";
echo strlen($stringa); // Output: 17 (considerando gli spazi)
$stringa = trim($stringa);
echo strlen($stringa); // Output: 11 (senza gli spazi)
```

## Approfondimento

È importante comprendere che `strlen()` restituisce il numero di byte nella stringa, non il numero di caratteri. Questo potrebbe causare problemi con le stringhe che contengono caratteri speciali multibyte, come quelli utilizzati nella lingua italiana.

Per risolvere questo problema, puoi utilizzare la funzione `mb_strlen()` che considera il numero corretto di caratteri multibyte nella stringa. Questa funzione accetta un parametro aggiuntivo per specificare il set di caratteri utilizzato, che di solito è `UTF-8` per il codice PHP. Ecco un esempio di utilizzo di `mb_strlen()`:

```PHP
$stringa = "Ciao";
echo strlen($stringa); // Output: 4 (considerando solo i byte)
echo mb_strlen($stringa, "UTF-8"); // Output: 4 (considerando anche i caratteri multibyte)
```

È importante tenere presente che `mb_strlen()` è più lenta rispetto a `strlen()`, quindi utilizzala solo quando hai a che fare con caratteri multibyte.

## Vedi anche

- [Documentazione PHP su strlen()](https://www.php.net/manual/en/function.strlen.php)
- [Documentazione PHP su trim()](https://www.php.net/manual/en/function.trim.php)
- [Documentazione PHP su mb_strlen()](https://www.php.net/manual/en/function.mb-strlen.php)