---
title:                "Scrivere test"
html_title:           "PHP: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/writing-tests.md"
---

{{< edit_this_page >}}

# Cos'è e perché scriverle?

Scrivere i test è un processo importante per i programmatori. Ciò implica la creazione di codice aggiuntivo per testare il funzionamento del codice principale. I test aiutano i programmatori a verificare che il loro codice sia accurato e funzionante correttamente in varie situazioni, riducendo così il rischio di errori durante lo sviluppo e l'utilizzo dell'applicazione.

# Come si fa:

```PHP

// Definizione della funzione per testare
function add($num1, $num2) {
   return $num1 + $num2;
}

// Chiamata alla funzione e output
echo add(5, 3) . PHP_EOL; // Output: 8
echo add(2, -1) . PHP_EOL; // Output: 1
```

# Approfondimento:

I test sono diventati una pratica comune nella programmazione grazie all'approccio di sviluppo guidato dai test (TDD) introdotto negli anni '90. Questa metodologia prevede la scrittura dei test prima del codice stesso, al fine di guidare lo sviluppo e garantire che ogni componente funzioni come previsto.

Esistono varie alternative ai test automatizzati, come i test manuali o i test di accettazione, ma la scrittura di test automatizzati è diventata una pratica essenziale per la maggior parte dei progetti software moderni. Inoltre, esistono numerose librerie e framework di testing disponibili in PHP, tra cui PHPUnit, Codeception e Behat.

Per implementare correttamente i test automatizzati, è importante seguire alcune best practice come scrivere test atomici, mantenere una buona copertura dei test e concentrarsi sui casi di test più importanti.

# Vedi anche:

- [PHPUnit documentazione](https://phpunit.de/manual/6.5/en/index.html)
- [Codeception documentazione](https://codeception.com/docs/)
- [Behat documentazione](https://docs.behat.org/en/latest/)