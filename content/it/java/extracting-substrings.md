---
title:                "Java: Estrazione di sottostringhe."
simple_title:         "Estrazione di sottostringhe."
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Java, probabilmente avrai incontrato situazioni in cui hai bisogno di estrarre una parte di una stringa più lunga. Può sembrare un compito semplice, ma c'è molto di più dietro l'estrazione di substringhe. In questo post, esploreremo il concetto di estrarre substringhe in dettaglio e vedremo come può essere utile per te nel tuo lavoro di sviluppo.

## Come

Estrarre substringhe in Java è possibile utilizzando il metodo `substring()` della classe `String`. Prende due parametri: l'indice iniziale e l'indice finale della parte della stringa che si desidera estrarre. Vediamo un esempio di come utilizzarlo:

```
String stringa = "Questo è un esempio di stringa";
String sub = stringa.substring(10, 15);
System.out.println(sub);
```

Questo codice stamperà "esemp" poiché stiamo estraendo la parte della stringa che va dall'indice 10 (incluso) all'indice 15 (escluso). Se volessimo invece estrarre solo una parte della stringa a partire da un certo indice, possiamo passare solo un parametro:

```
String stringa = "Questo è un esempio di stringa";
String sub = stringa.substring(10);
System.out.println(sub);
```

In questo caso, verrà stampata la parte della stringa a partire dall'indice 10 fino alla fine. Puoi anche combinare questo metodo con altri metodi di manipolazione delle stringhe per ottenere il risultato desiderato.

## Deep Dive

Se vuoi esplorare ulteriormente il concetto di estrarre substringhe, ci sono diverse cose che puoi considerare. Ad esempio, il metodo `substring()` non modifica la stringa originale, ma restituisce una nuova stringa. Inoltre, è possibile gestire eccezioni se l'indice fornito non esiste nella stringa originale. Inoltre, ci sono diversi modi per specificare gli indici per estrarre le substringhe, come utilizzare una variabile o una costante. Sperimenta con questi concetti per approfondire la tua comprensione della manipolazione delle stringhe in Java.

## Vedi anche

- [Documentazione ufficiale di Java su `substring()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)
- [Tutorial su come utilizzare `substring()`](https://www.tutorialspoint.com/java/java_string_substring.htm)
- [Domande frequenti su `substring()`](https://www.geeksforgeeks.org/string-class-substring-method-in-java-with-examples/)