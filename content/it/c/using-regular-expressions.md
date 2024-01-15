---
title:                "Utilizzare le espressioni regolari"
html_title:           "C: Utilizzare le espressioni regolari"
simple_title:         "Utilizzare le espressioni regolari"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Se ti trovi a dover gestire grandi quantità di testo, magari in un'applicazione di analisi dei dati o in un sistema di ricerca, sicuramente ti sei trovato nella situazione di dover filtrare, cercare o sostituire una specifica stringa di caratteri. In questi casi, un'ottima soluzione è rappresentata dalle espressioni regolari, una serie di caratteri speciali che permettono di effettuare operazioni complesse sulle stringhe di testo.

## Come si usa

Le espressioni regolari sono disponibili in C tramite la libreria PCRE (Perl Compatible Regular Expressions). Dopo aver incluso la libreria con l'istruzione `#include <pcre.h>`, è possibile utilizzare la funzione `pcre_compile()` per compilare un'espressione regolare e la funzione `pcre_exec()` per applicarla ad una stringa. Vediamo un esempio di utilizzo:

```
#include <stdio.h>
#include <pcre.h>

int main()
{
    char *regex = "^Hello (\\w+)$"; // Espressione regolare
    char *input = "Hello world"; // Stringa di input
    char *matches[2]; // Array per i risultati

    // Compilazione dell'espressione regolare
    pcre *regexCompiled = pcre_compile(regex, 0, NULL, NULL, NULL);

    // Applicazione dell'espressione regolare
    int result = pcre_exec(regexCompiled, NULL, input, strlen(input), 0, 0, matches, 2);

    // Stampa del risultato
    if (result > 0)
    {
        printf("Ciao, %.*s!", matches[1] - matches[0], input + matches[0]);
    }
    else
    {
        printf("Nessuna corrispondenza trovata.");
    }

    return 0;
}
```

L'output di questo programma sarà "Ciao, world!". Spieghiamo brevemente il codice: nella prima riga definiamo l'espressione regolare, che significa "inizia con la parola Hello seguita da uno o più caratteri alfanumerici". Nelle righe seguenti, inclusa la funzione `main()`, viene compilata l'espressione regolare e viene applicata alla stringa di input. In caso di successo, la funzione `pcre_exec()` restituisce il numero di corrispondenze trovate e salva i risultati nell'array `matches`. Il cui primo elemento corrisponde all'intera corrispondenza e il secondo alla prima cattura, ovvero il testo compreso tra le parentesi nella nostra espressione regolare. Infine, stampiamo il risultato a schermo.

## Approfondimento

Le espressioni regolari offrono numerose funzionalità che permettono di effettuare operazioni sempre più specifiche sul testo da elaborare. Per esempio, è possibile utilizzare dei quantificatori per indicare il numero di ripetizioni di un determinato carattere o gruppo di caratteri, o ancora usare gli operatori alternativi per definire più varianti di una corrispondenza. Inoltre, le espressioni regolari supportano anche i caratteri di escape, per far fronte a casi in cui è necessario cercare caratteri speciali come `.` o `+`.

## Vedi anche

- [Documentazione PCRE in italiano](https://www.php.net/manual/it/book.pcre.php)
- [Tutorial introduttivo alle espressioni regolari in C](https://www.codeproject.com/Articles/492194/Basic-Regex-Tutorial-In-C)