---
title:                "Conversione di una stringa in minuscolo"
html_title:           "Java: Conversione di una stringa in minuscolo"
simple_title:         "Conversione di una stringa in minuscolo"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui potresti voler convertire una stringa in minuscolo in un programma Java. Ad esempio, potresti avere una stringa con lettere maiuscole e minuscole e desiderare uniformare l'intera stringa in un formato standard. Oppure, potresti dover confrontare due stringhe, ma non vuoi che le differenze di lettere maiuscole/minuscole influiscano sul risultato.

## Come fare
Per convertire una stringa in minuscolo in Java, è possibile utilizzare il metodo `toLowerCase()` della classe `String`. Vediamo un esempio di codice:

```java
String stringaOriginale = "Java è un linguaggio di programmazione";
String stringaMinuscola = stringaOriginale.toLowerCase();
System.out.println(stringaMinuscola);
```
Questo codice stamperebbe "java è un linguaggio di programmazione" come output.

## Approfondimento
Il metodo `toLowerCase()` converte ogni carattere della stringa in minuscolo utilizzando la tabella Unicode. Ciò significa che funziona con qualsiasi alfabeto, compresi caratteri speciali e accenti. Inoltre, è possibile concatenare più chiamate a `toLowerCase()` per applicare la conversione a una stringa più di una volta.

## Vedi anche
- [Documentazione ufficiale di Java per il metodo toLowerCase()](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html#toLowerCase())
- [Un esempio pratico di utilizzo di toLowerCase()](https://www.baeldung.com/java-string-lowercase)
- [Tutorial Java su come manipolare le stringhe](https://www.javatpoint.com/java-string)