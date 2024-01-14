---
title:                "PHP: Utilizzare le espressioni regolari"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché utilizzare le espressioni regolari in PHP?

Le espressioni regolari sono un potente strumento per manipolare e cercare testi all'interno di un documento. In PHP, sono spesso utilizzate per effettuare controlli sul formato di una stringa o per estrarre dati specifici da un testo. Con le espressioni regolari, è possibile effettuare operazioni complesse in modo più efficiente rispetto all'utilizzo di metodi di stringhe standard.

## Come utilizzare le espressioni regolari in PHP

Per utilizzare le espressioni regolari in PHP, è necessario utilizzare la funzione `preg_match()`. Questa funzione accetta due argomenti: il primo è il pattern di ricerca, ovvero l'espressione regolare stessa, mentre il secondo è la stringa in cui effettuare la ricerca. Ad esempio:

```PHP
$stringa = "Questo è un esempio di stringa che contiene un numero: 123456";

if (preg_match("/[0-9]+/", $stringa)) {
    echo "La stringa contiene almeno un numero.";
} else {
    echo "La stringa non contiene numeri.";
}
```

In questo esempio, stiamo cercando un numero all'interno della stringa utilizzando il pattern `[0-9]+`, che corrisponde ad una o più occorrenze di cifre da 0 a 9. La funzione `preg_match()` restituirà un valore booleano, vero se la stringa contiene almeno un numero e falso altrimenti.

## Approfondimenti sulle espressioni regolari

Per utilizzare appieno le espressioni regolari in PHP, è importante comprendere i diversi simboli e metacaratteri che possono essere utilizzati per creare pattern di ricerca più complessi e specifici. Alcuni di questi includono:

- `.`: corrisponde ad un singolo carattere qualsiasi
- `*`: corrisponde a 0 o più occorrenze del carattere precedente
- `+`: corrisponde ad 1 o più occorrenze del carattere precedente
- `?`: indica che il carattere precedente può essere opzionale
- `[]`: definisce un insieme di caratteri da cui selezionare uno solo
- `[^]`: nega gli elementi all'interno delle parentesi quadre
- `^`: indica che la corrispondenza deve avvenire all'inizio della stringa
- `$`: indica che la corrispondenza deve avvenire alla fine della stringa

È possibile combinare questi simboli e metacaratteri per creare pattern di ricerca personalizzati. Ad esempio, possiamo utilizzare `[a-zA-Z]+` per cercare una o più occorrenze di lettere maiuscole e minuscole all'interno di una stringa.

## Vedi anche

- [Documentazione ufficiale di PHP sulle espressioni regolari](https://www.php.net/manual/en/book.pcre.php)
- [Esempi di espressioni regolari in PHP](https://www.php.net/manual/en/regexp.reference.php)
- [Online regex tester](https://regex101.com/)