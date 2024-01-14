---
title:                "Java: Eliminazione di caratteri corrispondenti a uno schema"
simple_title:         "Eliminazione di caratteri corrispondenti a uno schema"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

In questo post, parleremo di come eliminare i caratteri che corrispondono ad uno specifico pattern in una stringa in Java. Ciò può essere utile quando si desidera rimuovere informazioni indesiderate o sanificare l'input dell'utente prima di elaborarlo.

## Come fare

Per eliminare i caratteri che corrispondono ad un pattern, possiamo utilizzare il metodo `replaceAll()` della classe `String` in Java. Questo metodo accetta due parametri: il primo è il pattern che vogliamo sostituire, mentre il secondo è la stringa con cui vogliamo sostituire il pattern corrispondente. Vediamo un esempio:

```Java
String testo = "Hello, Java programmers!";
String nuovoTesto = testo.replaceAll("[aeiou]", "");
System.out.println(nuovoTesto);
```

In questo esempio, stiamo eliminando tutte le vocali dalla stringa `testo` e il risultato stampato sarà "Hll, Jv prgrmmrs!".

## Approfondimento

Ma qual è il significato di `[aeiou]` nel nostro esempio? Questo è un esempio di espressione regolare, un sistema di pattern matching usato per identificare stringhe di testo che corrispondono a uno specifico pattern. Nel nostro caso, `[aeiou]` indica tutte le vocali dell'alfabeto inglese.

Possiamo usare anche altre espressioni regolari per eliminare caratteri specifici. Ad esempio, se vogliamo eliminare tutti i numeri da una stringa, possiamo utilizzare `\\d`, che indica tutti i numeri da 0 a 9. Per approfondire l'utilizzo delle espressioni regolari in Java, puoi consultare [questa guida](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html).

## Vedi anche

- [Java String documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Regular Expressions in Java](https://www.javatpoint.com/java-regex)