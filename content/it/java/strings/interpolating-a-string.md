---
date: 2024-01-20 17:51:03.376959-07:00
description: "Come Farlo: Prima di Java 15, l'interpolazione non era cos\xEC semplice.\
  \ Si usava la concatenazione (`+`) o `String.format()`. Con Java 15, le stringhe\u2026"
lastmod: '2024-04-05T21:53:44.067164-06:00'
model: gpt-4-1106-preview
summary: "Prima di Java 15, l'interpolazione non era cos\xEC semplice."
title: Interpolazione di una stringa
weight: 8
---

## Come Farlo:
```java
public class InterpolationDemo {
    public static void main(String[] args) {
        // Esempio con Java 15 e superiori
        String name = "Marco";
        int age = 25;
        
        // Template string con text blocks
        String greeting = """
                          Ciao, %s!
                          Hai %d anni.
                          """.formatted(name, age);
        
        System.out.println(greeting);
    }
}

/*
Output:
Ciao, Marco!
Hai 25 anni.
*/
```

## Approfondimento
Prima di Java 15, l'interpolazione non era così semplice. Si usava la concatenazione (`+`) o `String.format()`. Con Java 15, le stringhe multiriga (text blocks) e il metodo `formatted()` hanno semplificato molto le cose.

Esistono alternative: oltre a `String.format()` e la concatenazione, si può usare `MessageFormat` o librerie esterne come Apache Commons Lang `StrSubstitutor`.

Dal punto di vista dell'implementazione, `String.format()` utilizza un pattern `Formatter` che può incidere sulle prestazioni se usato impropriamente. Invece, l'uso di text blocks con `formatted()` tende a essere più leggibile e performante per le stringhe statiche.

## Vedi Anche
- [Documentazione ufficiale Oracle sulle Stringhe](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Java Enhancement Proposal 355: Text Blocks (Second Preview)](https://openjdk.java.net/jeps/355)
- [Java Enhancement Proposal 378: Text Blocks](https://openjdk.java.net/jeps/378)
- [Apache Commons Lang StrSubstitutor](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/text/StrSubstitutor.html)
