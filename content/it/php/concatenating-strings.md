---
title:                "PHP: Unione di stringhe"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché 

La concatenazione di stringhe è un'operazione comune nella programmazione di PHP ed è fondamentale per la gestione di dati e la creazione di testo dinamico. Utilizzando questo concetto, è possibile combinare più stringhe di testo in una sola, consentendo al nostro codice di essere più efficiente e versatile.

## Come Fare

La concatenazione di stringhe in PHP è semplice e diretta. Possiamo utilizzare l'operatore di concatenazione "." per combinare due o più stringhe in una sola. Ad esempio, se abbiamo le stringhe "Ciao" e "Mondo", possiamo concatenarle con il seguente codice:

```PHP
$stringa1 = "Ciao";
$stringa2 = "Mondo";
$stringa3 = $stringa1 . $stringa2; // Risultato: "CiaoMondo"
```

Possiamo anche utilizzare l'operatore di assegnazione combinato ".=" per concatenare una stringa a un'altra in modo più efficiente. Ad esempio:

```PHP
$stringa1 .= " a tutti"; // Risultato: "Ciao a tutti"
```

Se vogliamo aggiungere uno spazio tra le due stringhe, possiamo semplicemente aggiungerlo come parte della stringa di concatenazione. Ad esempio:

```PHP
$stringa1 = $stringa1 . " " . $stringa2; // Risultato: "Ciao Mondo"
```

Inoltre, possiamo anche concatenare valori di variabili all'interno di una stringa utilizzando le parentesi graffe {}. Ad esempio:

```PHP
$nome = "Mario";
$saluto = "Ciao { $nome }"; // Risultato: "Ciao Mario"
```

## Approfondimento

Oltre all'operatore di concatenazione, ci sono altri metodi per manipolare stringhe in PHP. Ad esempio, la funzione `concat()` ci permette di concatenare un numero variabile di stringhe all'interno di una sola chiamata di funzione. Possiamo anche utilizzare la funzione `sprintf()` per formattare una stringa con valori di variabili. Inoltre, utilizzando l'operatore di concatenazione inversa "->", possiamo concatenare proprietà di oggetti in PHP.

## Vedi Anche

- [Documentazione di PHP sulla concatenazione di stringhe](https://www.php.net/manual/en/language.operators.string.php)
- [Tutorial su come manipolare stringhe in PHP](https://www.w3schools.com/php/php_strings.asp)
- [Esempi di concatenazione di stringhe in PHP](https://www.tutorialspoint.com/php/php_string_concatenation.htm)