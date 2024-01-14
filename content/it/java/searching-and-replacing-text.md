---
title:                "Java: Ricerca e sostituzione di testo"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché
Provare a cercare e sostituire del testo può sembrare un compito noioso o insignificante, ma in realtà è un'abilità fondamentale per qualsiasi programmatore Java. Sapere come implementare questa tecnica è importante per migliorare l'efficienza e la precisione del tuo codice.

## Come fare
Per eseguire la ricerca e la sostituzione di testo in Java, puoi utilizzare il metodo `replace()` della classe `String`. Questo metodo prende due argomenti, il testo da cercare e il testo con cui sostituirlo. Ad esempio, se volessimo sostituire la parola "cane" con "gatto" all'interno di una stringa, sarebbe sufficiente scrivere:

```Java
String testo = "Io ho un cane";
String nuovoTesto = testo.replace("cane", "gatto");
System.out.println(nuovoTesto);
```

Questo codice produrrà l'output "Io ho un gatto". Se vuoi sostituire tutte le occorrenze di una parola, puoi utilizzare il metodo `replaceAll()` invece di `replace()`, come mostrato nell'esempio seguente:

```Java
String testo2 = "Il mio cane si chiama Fido. Il cane del mio vicino si chiama Rex";
String nuovoTesto2 = testo2.replaceAll("cane", "gatto");
System.out.println(nuovoTesto2);
```

Questo produrrà l'output "Il mio gatto si chiama Fido. Il gatto del mio vicino si chiama Rex".

## Approfondimento
Oltre al metodo `replace()` e `replaceAll()`, Java offre anche alcuni altri metodi utili per la ricerca e la sostituzione di testo. Ad esempio, il metodo `startsWith()` verifica se una stringa inizia con un determinato testo, mentre `endsWith()` verifica se una stringa termina con quel testo. Il metodo `contains()` verifica se una stringa contiene un certo testo.

Inoltre, puoi utilizzare espressioni regolari per effettuare ricerche e sostituzioni più avanzate. Java fornisce la classe `Pattern` e `Matcher` per lavorare con le espressioni regolari. Puoi trovare ulteriori informazioni su come utilizzarle nella [documentazione ufficiale di Java](https://docs.oracle.com/javase/tutorial/essential/regex/).

## Vedi anche
- [Java String Documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java Regex Tutorial](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)