---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

L'interpolazione delle stringhe è un processo che sostituisce segnaposti all'interno di una stringa con i loro valori corrispondenti. Lo facciamo per creare stringhe dinamiche, migliorare la leggibilità del codice e ridurre il rischio di errori.

## Come fare:

La versione corrente di Java non supporta nativamente l'interpolazione delle stringhe, ma possiamo usare `String.format()`, come mostrato:

```Java
int eta = 26;
String nome = "Luca";
String messaggio = String.format("Ciao, il mio nome è %s e ho %d anni", nome, eta);
System.out.println(messaggio);
```

L'output sarà:

```Java
Ciao, il mio nome è Luca e ho 26 anni
```

## Approfondimento:

L'interpolazione delle stringhe è un concetto comune in molti linguaggi di programmazione come JavaScript e Python. Prima di Java 5, si ricorreva alla concatenazione di stringhe o a `StringBuffer` o `StringBuilder`.

Un'alternativa all'uso di `String.format()` è utilizzare una libreria esterna, come `MessageFormat` di Apache Commons.

Dettagli implementativi: `String.format()` utilizza internamente `Formatter` che interpreta il formato della stringa. Ogni volta che si chiama `String.format()`, viene creato un nuovo oggetto `Formatter`, il che può portare a un overhead importante nell'utilizzo della memoria se la chiamata avviene in un ciclo pesante.

## Vedi anche:

- Documentazione ufficiale di Java su [String.format()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#format-java.lang.String-java.lang.Object...-)
- Documentazione di [MessageFormat](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/text/MessageFormat.html) da Apache Commons.