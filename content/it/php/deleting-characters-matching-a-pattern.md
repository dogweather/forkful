---
title:                "PHP: Eliminare caratteri corrispondenti ad un modello"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui può essere necessario eliminare i caratteri che corrispondono ad un certo modello. Una delle più comuni è la pulizia dei dati, ad esempio rimuovere spazi, caratteri speciali o simboli non necessari da stringhe di testo o dati provenienti da form di input.

## Come fare

Per eliminare i caratteri che corrispondono ad un modello in PHP, possiamo utilizzare la funzione `preg_replace()` che consente di sostituire occorrenze di un pattern con una stringa vuota. Di seguito un esempio con codice e output:

```PHP
$testo = "Questa è una frase di prova12345!@#$";

// Utilizziamo il pattern '/[0-9\W]+/' per identificare i numeri e i caratteri speciali
$nuovo_testo = preg_replace('/[0-9\W]+/', "", $testo);

// Output: "Questaèunafrasediprova"
```

Possiamo anche utilizzare le espressioni regolari per specificare modelli più complessi. Ad esempio, se vogliamo eliminare tutti i numeri all'interno di parentesi tonde, possiamo usare il pattern `/\(\d+\)/` che corrisponde a qualsiasi numero racchiuso tra parentesi tonde. Di seguito un esempio:

```PHP
$testo = "Questo è un (12)esempio (345) di testo (6789)";

// Utilizziamo il pattern '/\(\d+\)/' per identificare le parentesi tonde con numeri all'interno
$nuovo_testo = preg_replace('/\(\d+\)/', "", $testo);

// Output: "Questoèun esempio di testo"
```

## Approfondimento

L'utilizzo delle espressioni regolari in PHP è molto potente e versatile. Con esse è possibile specificare qualsiasi tipo di modello che si vuole cercare e manipolare all'interno di una stringa. E' importante però fare attenzione ai caratteri speciali che possono alterare il risultato finale. Ad esempio, il carattere `\` deve essere utilizzato per far "scappare" caratteri speciali come `/` o `(`.

Per saperne di più sull'utilizzo delle espressioni regolari in PHP, puoi dare un'occhiata alla documentazione ufficiale della funzione `preg_replace()` e ai vari esempi presenti online.

## Vedi Anche

- Documentazione ufficiale di `preg_replace()`: https://www.php.net/manual/en/function.preg-replace.php
- Esempi di espressioni regolari per PHP: https://www.tutorialspoint.com/php/php_regular_expression.htm
- Tutorial su come utilizzare espressioni regolari in PHP: https://www.w3schools.com/php/php_regex.asp