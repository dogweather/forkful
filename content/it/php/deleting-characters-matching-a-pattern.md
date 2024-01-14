---
title:                "PHP: Eliminazione di caratteri corrispondenti a un modello"
simple_title:         "Eliminazione di caratteri corrispondenti a un modello"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Nella programmazione, è spesso necessario modificare una stringa rimuovendo specifici caratteri. Questo può essere utile, ad esempio, per la pulizia dei dati o per la formattazione di input utente. In questo articolo, esploreremo come eliminare i caratteri che corrispondono a un certo pattern utilizzando PHP.

## Come fare

Per eliminare i caratteri che corrispondono a un pattern, utilizzeremo la funzione `preg_replace()` di PHP. Questa funzione prende tre parametri: il pattern da cercare, il valore che lo sostituirà e la stringa di input. Ad esempio, se vogliamo eliminare tutte le lettere minuscole da una stringa, possiamo utilizzare il seguente codice:

```PHP
$input = "Questa è una stringa di testo!";
$output = preg_replace('/[a-z]/', '', $input);
echo $output;
```
```
Q
```

Notate come il risultato finale sia una sola lettera, la lettera maiuscola "Q". Questo perché il pattern `[a-z]` corrisponde a qualsiasi lettera minuscola all'interno della stringa. Possiamo anche utilizzare altri pattern, come ad esempio `[0-9]` per rimuovere tutti i numeri, o `[^\w]` per eliminare tutti i caratteri non alfanumerici.

## Deep Dive

La funzione `preg_replace()` utilizza le espressioni regolari, una potente tecnica per la ricerca e la manipolazione di pattern all'interno di una stringa. In questo caso, stiamo utilizzando un pattern tra parentesi quadre `[]` che indica una classe di caratteri. Il simbolo `^` all'interno della classe indica una negazione, quindi `[^\w]` corrisponde a qualsiasi carattere che non sia una lettera, un numero o l'underscore `_`.

Per saperne di più sulle espressioni regolari e su come utilizzarle in PHP, è possibile consultare le seguenti risorse:

- [Documentazione ufficiale di PHP](https://www.php.net/manual/en/reference.pcre.pattern.syntax.php)
- [RegexOne](https://regexone.com/) (tutorial interattivo sulle espressioni regolari)
- [RegExr](https://regexr.com/) (strumento online per testare ed esplorare le espressioni regolari)

## Vedi anche

- [PHP: Built-in Functions - preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP: Regular Expressions](https://www.php.net/manual/en/book.pcre.php)
- [PHP Tutorial - Regular Expressions](https://www.w3schools.com/php/php_regex.asp) (tutorial su w3schools)