---
title:    "PHP: Scrivere su standard error"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Perché Scrivere su Standard Error?

Spesso, quando si programma in PHP, ci troviamo nella situazione in cui dobbiamo gestire eventuali errori o avvisi che possono verificarsi durante l'esecuzione del codice. In questi casi, scrivere su standard error può essere un'opzione utile per gestire tali situazioni in modo efficace.

## Come Fare

Per scrivere su standard error in PHP, possiamo utilizzare la funzione nativa "fwrite", che ci permette di inviare un messaggio di testo al file di output di errore del server. Vediamo un esempio di codice:

```PHP
<?php

// Apre il file di output di errore in modalità "append", per aggiungere nuovi messaggi senza sovrascrivere quelli precedenti
$fp = fopen('php://stderr', 'a');

// Scrive il messaggio sulla console degli errori
fwrite($fp, "Attenzione! Questo è un messaggio di errore.");

// Chiude il file
fclose($fp);
```

Il codice sopra ci permette di scrivere un messaggio di errore personalizzato sulla console degli errori, invece di visualizzarlo sulla pagina web. Questo è particolarmente utile quando vogliamo mantenere i nostri errori privati e non farli vedere agli utenti del sito.

## Deep Dive

La funzione "fwrite" accetta due parametri: il primo è il file di output su cui scrivere, mentre il secondo è il messaggio che vogliamo inviare. Per scrivere su standard error, utilizziamo "php://stderr" come file di output. 

Inoltre, possiamo anche utilizzare questa tecnica per scrivere informazioni di debug o avvisi sul file di output di errori. Ciò ci permette di tenere traccia dei vari passaggi eseguiti dal codice durante l'esecuzione e di risolvere eventuali errori in modo più rapido e preciso.

## Vedi Anche

- [Documentazione ufficiale PHP su fwrite](https://www.php.net/manual/en/function.fwrite.php)
- [Guida su come gestire gli errori in PHP](https://www.php.net/manual/en/book.errorfunc.php)
- [Esempio di utilizzo di fwrite per scrivere su standard error](https://stackoverflow.com/questions/893011/how-to-write-to-standard-error-in-php)