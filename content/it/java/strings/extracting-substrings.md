---
date: 2024-01-20 17:45:43.340594-07:00
description: "Estrarre sottostringhe significa selezionare parti specifiche di una\
  \ stringa di testo. I programmatori lo fanno per manipolare e analizzare i dati,\
  \ come\u2026"
lastmod: 2024-02-19 22:05:02.358856
model: gpt-4-1106-preview
summary: "Estrarre sottostringhe significa selezionare parti specifiche di una stringa\
  \ di testo. I programmatori lo fanno per manipolare e analizzare i dati, come\u2026"
title: Estrazione di sottostringhe
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
