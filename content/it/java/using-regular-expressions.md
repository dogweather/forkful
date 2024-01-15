---
title:                "Utilizzo delle espressioni regolari"
html_title:           "Java: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Java, probabilmente hai sentito parlare delle espressioni regolari (o "regex"). Questi strumenti sono utilizzati per cercare e manipolare testo in un modo potente e flessibile. Se stai lavorando con dati testuali, le espressioni regolari possono semplificare notevolmente il tuo codice.

## Come

Le espressioni regolari sono supportate dal pacchetto `java.util.regex`, che fornisce una classe `Pattern` per compilare e gestire espressioni regolari, e una classe `Matcher` per applicarle a del testo. Ecco un esempio di come cercare una stringa in un testo utilizzando una espressione regolare:

```java
String testo = "Le espressioni regolari possono semplificare il tuo codice.";
Pattern pattern = Pattern.compile("regex");
Matcher matcher = pattern.matcher(testo);

if (matcher.find()) {
  System.out.println("Trovata una corrispondenza per la parola 'regex'!");
}
```

In questo caso, il programma stamperebbe "Trovata una corrispondenza per la parola 'regex'!" poiché la stringa `testo` contiene la parola "espressioni regolari". Oltre alla ricerca di stringhe, le espressioni regolari consentono di eseguire operazioni come sostituire del testo e dividere stringhe in base a un pattern specifico.

## Approfondimento

Usare le espressioni regolari richiede un po' di pratica e familiarità con la loro sintassi. La documentazione ufficiale di Java fornisce una guida completa alle espressioni regolari e ai loro metodi. Inoltre, ci sono molti tutorial e esempi online che possono aiutarti a padroneggiare questa utile tecnica di manipolazione del testo.

## Vedi anche

- [Documentazione ufficiale di Java sulle espressioni regolari](https://docs.oracle.com/javase/8/docs/api/java/util/regex/package-summary.html)
- [Tutorial di TutorialsPoint sulle espressioni regolari in Java](https://www.tutorialspoint.com/java/java_regular_expressions.htm)
- [Esempi di regex su regex101.com](https://regex101.com/) (in inglese)