---
date: 2024-01-20 17:45:43.340594-07:00
description: "How to: (Come fare:) Il metodo `substring` di Java \xE8 l'evoluzione\
  \ di funzioni simili esistenti dai primi giorni del linguaggio (anni '90). Altre\u2026"
lastmod: '2024-04-05T22:50:57.126199-06:00'
model: gpt-4-1106-preview
summary: "(Come fare:) Il metodo `substring` di Java \xE8 l'evoluzione di funzioni\
  \ simili esistenti dai primi giorni del linguaggio (anni '90)."
title: Estrazione di sottostringhe
weight: 6
---

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
