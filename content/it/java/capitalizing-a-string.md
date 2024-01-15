---
title:                "Capitalizzare una stringa"
html_title:           "Java: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché
 Se stai scrivendo codice Java, è molto probabile che a volte ti trovi a dover capitalizzare una stringa. Questa operazione è comune in molti casi, ad esempio quando si deve mostrare un testo in formato titolo o quando si intende effettuare un confronto di stringhe in modo case-insensitive.

## Come fare
 Per capitalizzare una stringa in Java, puoi utilizzare il metodo `toUpperCase()` della classe `String`. Basta semplicemente passare la stringa che vuoi capitalizzare come parametro e il metodo restituirà una nuova stringa con tutti i caratteri in maiuscolo. Ad esempio:

```Java
String s1 = "ciao a tutti";
String s2 = s1.toUpperCase();

System.out.println(s2);
```

Output:
```
CIAO A TUTTI
```

È importante notare che il metodo `toUpperCase()` non modifica direttamente la stringa originale, ma ne restituisce una nuova. Se vuoi che la stringa originale venga modificata, puoi utilizzare il metodo `toUpperCase(Locale)` e passare come parametro una specifica località linguistica. Ad esempio:

```Java
String s1 = "hello";
String s2 = s1.toUpperCase(Locale.ITALIAN);

System.out.println(s2);
```

Output:
```
HELLO
```

## Approfondimento
 Se vuoi conoscere meglio come funziona l'operazione di capitalizzazione di una stringa in Java, devi prima capire che i caratteri alfanumerici (a-z, A-Z, 0-9) sono rappresentati all'interno del computer tramite un codice numerico. Ad esempio, la lettera "a" ha un codice corrispondente di 97, mentre la lettera "A" ha un codice di 65. Quando viene eseguita l'operazione `toUpperCase()`, il codice della lettera viene modificato sommando o sottraendo una costante, a seconda se si tratta di una lettera minuscola o maiuscola. La costante utilizzata è 32, quindi per convertire la lettera "a" in "A", si sottrae 32 dal suo codice (97 - 32 = 65).

## Vedi anche
- [Documentazione ufficiale Java per la classe String](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html)
- [Java String toUpperCase() method tutorial](https://www.journaldev.com/17409/java-string-touppercase)