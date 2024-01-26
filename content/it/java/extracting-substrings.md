---
title:                "Estrazione di sottostringhe"
date:                  2024-01-20T17:45:43.340594-07:00
model:                 gpt-4-1106-preview
simple_title:         "Estrazione di sottostringhe"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Estrarre sottostringhe significa selezionare parti specifiche di una stringa di testo. I programmatori lo fanno per manipolare e analizzare i dati, come estrarre nomi utente da indirizzi email o processare codici di prodotto.

## How to: (Come fare:)
```Java
public class EstrattoreDiSottostringhe {
    public static void main(String[] args) {
        String frase = "Ciao, mondo di Java!";

        // Estrai una sottostringa utilizzando substring(int beginIndex, int endIndex)
        String parola = frase.substring(6, 11);
        System.out.println(parola); // Output: mondo

        // Ottieni il resto della stringa da un indice fino alla fine
        String resto = frase.substring(12);
        System.out.println(resto); // Output: di Java!
    }
}
```

## Deep Dive (Approfondimento)
Il metodo `substring` di Java è l'evoluzione di funzioni simili esistenti dai primi giorni del linguaggio (anni '90). Altre alternative per l'estrazione di sottostringhe includono l'uso di espressioni regolari (`Pattern` e `Matcher`) o altre API come `StringUtils` di Apache Commons. Dettagli di implementazione: fare attenzione a `StringIndexOutOfBoundsException` quando gli indici sono fuori dai limiti della stringa e ricordare che `substring(int beginIndex)` va fino alla fine della stringa.

## See Also (Vedi Anche)
- [String.substring(int beginIndex, int endIndex)](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#substring(int,int))
- [Class Pattern](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [Apache Commons StringUtils](https://commons.apache.org/proper/commons-lang/)
