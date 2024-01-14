---
title:    "PHP: Eliminazione di caratteri corrispondenti a un modello"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Perché

Cancellare i caratteri che corrispondono ad un particolare schema può essere utile in diverse situazioni di programmazione. Ad esempio, per pulire una stringa di input o per filtrare dati non desiderati da un array.

## Come fare

Per effettuare questa operazione in PHP, possiamo utilizzare la funzione `preg_replace()`. Questa funzione prende come parametri l'espressione regolare che descrive il pattern da eliminare, il rimpiazzo desiderato e la stringa di input su cui effettuare la sostituzione. 

```PHP
$input = "14-06-2020";
$output = preg_replace('/[^0-9]/s', '', $input);
echo $output;
```
Nell'esempio sopra, stiamo eliminando tutti i caratteri che non sono numeri dalla stringa "14-06-2020", ottenendo così il risultato "14062020".

## Approfondimento

La funzione `preg_replace()` utilizza le espressioni regolari per identificare il pattern da eliminare nella stringa di input. Per chi non è familiare con le espressioni regolari, queste sono delle sequenze di caratteri che descrivono uno schema preciso. Ad esempio, `[0-9]` indica tutti i numeri da 0 a 9, mentre `[^0-9]` indica tutti i caratteri che non sono numeri. Possiamo utilizzare queste espressioni per eliminare caratteri speciali, lettere e simboli specifici da una stringa.

## Vedi anche

- [Documentazione ufficiale di preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [Guida alle espressioni regolari in PHP](https://www.php.net/manual/en/reference.pcre.pattern.syntax.php)
- [Tutorial di espressioni regolari su Codecademy](https://www.codecademy.com/learn/learn-regular-expressions)