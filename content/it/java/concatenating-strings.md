---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
La concatenazione di stringhe in Java è il processo di unione di due o più stringhe in una unica stringa. I programmatori fanno ciò per costruire messaggi di output dettagliati, creare query SQL, e in molti altri scenari.

## Come si fa:
La concatenazione di stringhe può avvenire in diversi modi. L'operatore ‘+’ è uno dei più utilizzati, ma anche l'uso di StringBuilder o StringBuffer sono valide opzioni.

Qui ci sono degli esempi di come concatenare stringhe:

```Java
// Utilizzo dell'operatore '+'
String stringa1 = "Ciao, ";
String stringa2 = "come stai?";
String saluto = stringa1 + stringa2;
System.out.println(saluto);  // Output: "Ciao, come stai?"

// Utilizzo di StringBuilder
StringBuilder sb = new StringBuilder();
sb.append("Ciao, ");
sb.append("come stai?");
System.out.println(sb.toString());  // Output: "Ciao, come stai?"
```

## Approfondimenti
1. **Contesto storico**: La concatenazione di stringhe è presente da quando Java è stato introdotto. Le performance dell'operazione di concatenazione hanno migliorato con l'introduzione di StringBuilder e StringBuffer.

2. **Alternative**: L'operatore ‘+’ è facile da usare, ma può essere inefficiente quando ci sono molte stringhe da concatenare. In questi casi, è preferibile utilizzare StringBuilder o StringBuffer.

3. **Dettagli di Implementazione**: Quando si utilizza l'operatore '+', Java utilizza internamente una StringBuffer per gestire la concatenazione.

## Per approfondire
1. [Java String Concatenation: Good Practices](https://www.baeldung.com/java-strings-concatenation)
2. [Java Documentation: StringBuilder](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/StringBuilder.html)
3. [Java Documentation: StringBuffer](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/StringBuffer.html)