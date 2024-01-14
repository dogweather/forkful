---
title:    "PHP: Concatenazione di stringhe"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Vi è mai capitato di dover unire due o più stringhe di testo in un unico output? Questa è una delle funzionalità più comuni e utili della programmazione, soprattutto quando si lavora con dati dinamici. Concatenare le stringhe è un'operazione fondamentale per creare output dinamici e personalizzati all'interno di un programma.

## Come Fare

La concatenazione di stringhe in PHP è un'operazione molto semplice e intuitiva. Utilizzando l'operatore di concatenazione " . " è possibile unire più stringhe in un unico output. Ad esempio:

```PHP
$stringa1 = "Ciao";
$stringa2 = "a tutti";
$stringa3 = "!";
$stringa_finale = $stringa1 . $stringa2 . $stringa3;

echo $stringa_finale;
```
Output: Ciao a tutti!

Si noti che l'operatore di concatenazione può essere utilizzato anche all'interno di una stringa, come nel seguente esempio:

```PHP
echo "Benvenuto " . $nome . "!";
```
Output: Benvenuto [nome]!

## Approfondimento

La concatenazione di stringhe può essere utilizzata anche per manipolare o formattare i dati all'interno delle stringhe stesse. Ad esempio, è possibile utilizzare la funzione `strtoupper()` per convertire una stringa in maiuscolo prima di concatenarla:

```PHP
$stringa1 = "ciao";
$stringa2 = "a tutti";
$stringa3 = "!";
$stringa_finale = strtoupper($stringa1) . $stringa2 . $stringa3;

echo $stringa_finale;
```
Output: CIAO a tutti!

La concatenazione può anche essere utilizzata insieme alle variabili di sistema PHP per creare URL dinamici o per costruire query di database personalizzate.

## Vedi Anche

- [Guida PHP alla manipolazione delle stringhe](https://www.php.net/manual/en/language.types.string.php)
- [PHP concatenation operator](https://www.php.net/manual/en/language.operators.string.php) 
- [PHP string functions](https://www.php.net/manual/en/ref.strings.php)