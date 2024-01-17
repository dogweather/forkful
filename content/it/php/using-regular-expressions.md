---
title:                "Utilizzare le espressioni regolari"
html_title:           "PHP: Utilizzare le espressioni regolari"
simple_title:         "Utilizzare le espressioni regolari"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Che cos'è e perché si usano le espressioni regolari?
Le espressioni regolari sono una potente funzionalità che permette ai programmatori di manipolare e cercare pattern di testo in modo efficiente. Questo è particolarmente utile quando si lavora con grandi quantità di dati o quando si deve cercare un certo tipo di informazione all'interno di un documento. Le espressioni regolari sono ampiamente utilizzate in quasi tutti i linguaggi di programmazione, inclusa la versione attuale di PHP, per la loro versatilità e flessibilità.

## Come si utilizzano le espressioni regolari in PHP?
È possibile utilizzare le espressioni regolari in PHP utilizzando la funzione `preg_match()` o gli operatori di corrispondenza (ad esempio, `preg_match_all()` o `preg_replace()`) all'interno di un blocco di codice `PHP ...`. Ecco un esempio di come si potrebbe utilizzare `preg_match()` per cercare una sequenza di testo all'interno di una stringa:
```
PHP
$test_string = "Questa è una stringa di esempio.";
if (preg_match("/stringa/", $test_string)) {
   echo "Corrispondenza trovata!";
} else {
   echo "Nessuna corrispondenza trovata.";
}
```

**Output:**
```
Corrispondenza trovata!
```

## Scopri di più sulle espressioni regolari
Le espressioni regolari sono state sviluppate inizialmente negli anni '50 da Stephen Kleene come parte della teoria dei linguaggi formali. Nel tempo, sono state standardizzate e integrate in molti linguaggi di programmazione, fornendo un metodo versatile per cercare, manipolare e sostituire i pattern di testo. Mentre ci sono altre alternative per la manipolazione dei testi, come le funzioni di manipolazione delle stringhe di PHP, le espressioni regolari offrono una maggiore precisione e potenza.

## Vedi anche
- [PHP.net: Espressioni regolari](https://www.php.net/manual/en/regexp.reference.php)
- [W3Schools: Introduzione alle espressioni regolari in PHP](https://www.w3schools.com/php/php_regex.asp)
- [Tutsplus: Guida alle espressioni regolari in PHP](https://code.tutsplus.com/tutorials/8-regular-expressions-you-should-know--net-6149)