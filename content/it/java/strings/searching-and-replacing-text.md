---
date: 2024-01-20 17:58:07.932921-07:00
description: 'Come fare: Ecco un esempio semplice in Java che mostra come cercare
  e sostituire una stringa.'
lastmod: '2024-03-13T22:44:43.293994-06:00'
model: gpt-4-1106-preview
summary: Ecco un esempio semplice in Java che mostra come cercare e sostituire una
  stringa.
title: Ricerca e sostituzione del testo
weight: 10
---

## Come fare:
Ecco un esempio semplice in Java che mostra come cercare e sostituire una stringa.

```java
public class SearchReplace {

    public static void main(String[] args) {
        String originalText = "Le mele sono rosse e le banane sono gialle.";
        String searchText = "rosse";
        String replaceText = "verdi";

        String replacedText = originalText.replace(searchText, replaceText);

        System.out.println(replacedText);
    }
}
```

Output:
```
Le mele sono verdi e le banane sono gialle.
```

## Approfondimento:
Storicamente, il concetto di ricerca e sostituzione è stato introdotto nell'editing testuale e si è sviluppato come strumento comune negli IDE e negli editor di testo. In Java, `String` class introduce metodi come `replace()`, `replaceAll()`, e `replaceFirst()`. Le prime due varianti lavorano con stringhe semplici e espressioni regolari, rispettivamente. L'efficienza di queste operazioni può dipendere dalla lunghezza del testo e dalla complessità dell'espressione regolare.

Altre classi utili in Java per lavorare con testi includono `StringBuilder` e `Pattern` per compiti più avanzati come i MatchFinder.

## Vedi Anche:
- Documentazione ufficiale di Java `String` class: https://docs.oracle.com/javase/10/docs/api/java/lang/String.html
- Java `Pattern` class per espressioni regolari: https://docs.oracle.com/javase/10/docs/api/java/util/regex/Pattern.html
- Una guida alle espressioni regolari in Java: https://www.vogella.com/tutorials/JavaRegularExpressions/article.html
