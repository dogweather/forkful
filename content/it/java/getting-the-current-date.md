---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
La data corrente è semplicemente la data attuale, che viene utilizzata dai programmatori per diversi motivi. Ad esempio, possono essere utilizzati per registrare l'orario di esecuzione di un programma o per stampare la data corrente su uno schermo.

## Come fare:
```Java
import java.time.LocalDate;  // Importa la classe LocalDate per accedere alla data corrente
...
LocalDate currentDate = LocalDate.now(); // Crea una variabile che contiene la data corrente
System.out.println(currentDate); // Stampa la data corrente sulla console
```

Esempio di output:
```
2021-07-20
```

## Approfondimento:
La possibilità di ottenere la data corrente è stata introdotta in Java 8 attraverso la classe LocalDate. Esistono anche altre classi come Date e Calendar, ma sono state considerate obsolete e sostituite da LocalDate per una gestione più efficiente e senza problemi di fuso orario.

## Vedi anche:
- Documentazione di Java per LocalDate: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html
- Tutorial su come ottenere la data corrente in Java: https://www.baeldung.com/java-current-date