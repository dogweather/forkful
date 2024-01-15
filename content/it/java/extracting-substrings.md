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

## Perché

Se stai lavorando con stringhe in Java, potresti avere la necessità di estrarre una porzione di una stringa più grande, detta sottolineatura. Questo può essere utile per analizzare una stringa in input, manipolare un dato specifico o semplicemente per ottenere una parte di una stringa per usarla in seguito.

## Come fare

Per estrarre una sottolineatura da una stringa in Java, puoi utilizzare il metodo `substring()`. Questo metodo richiede due parametri, l'indice di inizio e la lunghezza della sottolineatura desiderata. Ad esempio:

```Java
String stringa = "Buongiorno a tutti!";
String sottolineatura = stringa.substring(0, 3);
System.out.println(sottolineatura); // Output: Buo
```

In questo esempio, stiamo estraendo una sottolineatura di lunghezza 3 a partire dall'indice 0 della stringa originale. Puoi anche specificare un solo parametro, l'indice di inizio, per estrarre una sottolineatura fino alla fine della stringa:

```Java
String stringa = "Buongiorno a tutti!";
String sottolineatura = stringa.substring(4);
System.out.println(sottolineatura); // Output: iorno a tutti!
```

Un'altra opzione è utilizzare il metodo `indexOf()` per trovare l'indice di una determinata sottostringa all'interno di una stringa. Questo può essere utile se non si conosce l'indice esatto di inizio della sottolineatura desiderata. Ad esempio:

```Java
String stringa = "Buongiorno a tutti!";
int indice = stringa.indexOf("giorno");
String sottolineatura = stringa.substring(indice, indice + 5); // Il secondo parametro rappresenta la lunghezza della sottostringa
System.out.println(sottolineatura); // Output: giorno
```

## Approfondimento

Estrarre sottolineature può diventare molto utile quando si lavora con input utente o si analizzano dati da fonti esterne. Il metodo `substring()` è molto versatile e consente di manipolare facilmente stringhe grandi o complesse. Inoltre, l'utilizzo del metodo `indexOf()` può semplificare la ricerca di una sottostringa all'interno di una stringa più grande.

## Vedi anche

- Documentazione ufficiale di Java su `substring()`: https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/lang/String.html#substring(int,int)
- Tutorial su come lavorare con stringhe in Java: https://www.w3schools.com/java/java_strings.asp