---
title:                "Ricerca e sostituzione del testo"
html_title:           "Arduino: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Che Cosa e Perché?
La ricerca e sostituzione di testo è un'operazione che permette di individuare stringhe di caratteri all'interno di un testo e di rimpiazzarle con altre. I programmatori la utilizzano per manipolare e pulire dati, automatizzare compiti, fare refactoring del codice e molto altro.

## Come fare:
La class String in Java ci offre due metodi per poter cercare e rimpiazzare del testo: `replace()` e `replaceAll()`. Ecco un esempio:

```Java
String s = "Buongiorno, mondo!";
// Sostituisce la prima occorrenza di "mondo" con "Italia"
s = s.replaceFirst("mondo", "Italia");
System.out.println(s); // Output: "Buongiorno, Italia!"

String t = "Pizza, Pasta, Pizza, Pizza!";
// Sostituisce tutte le occorrenze di "Pizza" con "Gelato"
t = t.replaceAll("Pizza", "Gelato");
System.out.println(t); // Output: "Gelato, Pasta, Gelato, Gelato!"
```

## Approfondimento:
La possibilità di cercare e rimpiazzare del testo è stata introdotta nei linguaggi di programmazione da molto tempo, per eseguire operazioni di manipolazione del testo in modo efficace.

In Java, `replace()` e `replaceAll()` usano differenti approcci: `replace()` opera un semplice rimpiazzo di stringhe, mentre `replaceAll()` utilizza le espressioni regolari, permettendo sostituzioni più elaborate.

C'è anche un'altra alternativa, `replaceFirst()`, che sostituisce solo la prima corrispondenza trovata. Decidere quale usare dipende dal tuo caso specifico.

Ricorda che in Java le stringhe sono immutabili: quando "modifichi" una stringa, in realtà ne crei una nuova con il contenuto modificato.

## Altro da Considerare:
Per approfondire la manipolazione di stringhe in Java, consulta le seguenti risorse:

- [Documentazione ufficiale della classe String](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [Guida alle espressioni regolari in Java](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
- [Metodo `replace()`](https://www.w3schools.com/java/ref_string_replace.asp)
- [Metodo `replaceAll()`](https://www.w3schools.com/java/ref_string_replaceall.asp)