---
title:                "Eliminazione di caratteri corrispondenti a un modello"
html_title:           "PHP: Eliminazione di caratteri corrispondenti a un modello"
simple_title:         "Eliminazione di caratteri corrispondenti a un modello"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
La cancellazione dei caratteri che corrispondono ad uno schema è un'operazione comune nella programmazione. Si tratta di una funzione che permette ai programmatori di manipolare dati al fine di ottenere risultati specifici.

## Come fare:
Per cancellare i caratteri che corrispondono ad uno schema in PHP, possiamo utilizzare la funzione `preg_replace()`. Ad esempio, se vogliamo eliminare tutti i numeri da una stringa, possiamo usare il seguente codice:

```
$stringa = "Codice123 di prova";
$stringa = preg_replace("/[0-9]/", "", $stringa);
echo $stringa; // output: "Codice di prova"
```

È importante notare che il parametro `"/[0-9]/"` rappresenta lo schema che vogliamo eliminare. In questo caso, si tratta di tutti i numeri da 0 a 9.

## Approfondimento:
La cancellazione dei caratteri che corrispondono ad uno schema ha origini nella teoria dei linguaggi formali e consiste nell'applicazione di espressioni regolari ai dati. Oltre alla funzione `preg_replace()`, in PHP esistono anche altre funzioni per manipolare dati in base a schemi, come ad esempio `preg_match()` e `preg_split()`. Inoltre, esistono anche strumenti esterni come `grep` e `sed` che svolgono funzioni simili.

## Vedi anche:
- [Manuale PHP sulla funzione `preg_replace()`](https://www.php.net/manual/en/function.preg-replace.php)
- [Documentazione di grep](https://www.gnu.org/software/grep/)
- [Documentazione di sed](https://www.gnu.org/software/sed/)