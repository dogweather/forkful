---
title:    "Gleam: Eliminazione di caratteri corrispondenti a un pattern"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Perché

Sai come può essere frustrante dover eliminare manualmente caratteri indesiderati all'interno di un file di testo? Beh, usando il linguaggio di programmazione Gleam, puoi facilmente eliminare tutti i caratteri corrispondenti a un determinato modello. Continua a leggere per scoprire come!

## Come Fare

Per iniziare, devi importare il modulo "Pattern" all'interno del tuo programma Gleam. Successivamente, usa la funzione "Pattern.match" per cercare una corrispondenza all'interno di una stringa.

```
Gleam import Pattern

Gleam let result = Pattern.match("Hello World", "o")
// La variabile result conterrà ["o", "o"]
```

Se vuoi eliminare effettivamente il carattere corrispondente, puoi usare il metodo "String.replace" insieme alla funzione "Pattern.match" come segue:

```
Gleam let clean_string = String.replace("Hello World", Pattern.match("Hello World", "o"), "")
// La variabile clean_string conterrà "Hell Wrld"
```

## Approfondimento

Questa semplice funzione può essere molto utile per la pulizia dei dati o per il riformattazione di un testo. È anche possibile utilizzarla per eliminare più di un carattere corrispondente contemporaneamente. Basta fornire più modelli all'interno della funzione "Pattern.match" in una lista:

```
Gleam let result = Pattern.match("Hello World", ["o", "l"])
// La variabile result conterrà ["o", "l", "l"]
```

Se desideri un controllo più avanzato sul formato del tuo testo, puoi anche utilizzare espressioni regolari all'interno della funzione "Pattern.match".

## Vedi Anche

- Documentazione ufficiale di Gleam: https://gleam.run/
- Tutorial su come utilizzare le espressioni regolari in Gleam: https://gleam.run/articles/regex.html
- Esempi di codice di Gleam su GitHub: https://github.com/gleam-lang