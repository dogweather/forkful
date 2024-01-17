---
title:                "Estrazione di sottostringhe"
html_title:           "Java: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Estrarre sottostringhe significa ottenere una parte di una stringa più grande, solitamente basandosi su un indice o un pattern. I programmatori spesso fanno questo per manipolare e gestire meglio i dati all'interno di una stringa.

## Come fare:

```Java
// Esempio di estrarre una sottostringa basata su un indice:
String stringa = "Questo è un esempio";
String sottostringa = stringa.substring(3);
System.out.println(sottostringa); // Output: sto è un esempio

// Esempio di estrarre una sottostringa basata su un pattern:
String parola = "ciao123";
String numero = parola.substring(4,6);
System.out.println(numero); // Output: 12
```

## Approfondimento:

Estrarre sottostringhe è diventato uno dei concetti fondamentali della programmazione grazie alla sua utlità nella manipolazione dei dati. Un'alternativa comune all'utilizzo del metodo `substring()` è l'utilizzo di espressioni regolari per trovare e sostituire parti specifiche delle stringhe.

## Vedi anche:

- Documentazione ufficiale di Java per il metodo `substring()`: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-
- Tutorial su espressioni regolari in Java: https://www.vogella.com/tutorials/Javaregex/article.html