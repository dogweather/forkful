---
title:                "Java: Utilizzare le espressioni regolari"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Usare le espressioni regolari è un modo efficace per cercare, validare e sostituire testo all'interno di una stringa. Se vuoi facilitare le tue attività di elaborazione del testo in Java, le espressioni regolari sono uno strumento essenziale da imparare.

## Come utilizzare le espressioni regolari in Java

Le espressioni regolari in Java sono supportate dal pacchetto `java.util.regex`, che fornisce un'ampia gamma di metodi per operare con le espressioni regolari. Per prima cosa, dobbiamo creare un oggetto `Pattern` utilizzando il metodo `compile` e specificando la stringa contenente la regex. Ad esempio:

```
Pattern regex = Pattern.compile("[a-z]+"); 
```

In questo caso, stiamo cercando di trovare una corrispondenza per qualsiasi sequenza di caratteri minuscoli nella nostra stringa. Successivamente, dobbiamo creare un oggetto `Matcher` utilizzando il metodo `matcher` e passando come argomento la stringa su cui vogliamo eseguire l'operazione. Ad esempio:

```
Matcher matcher = regex.matcher("Questo è il mio testo di prova");
```

Infine, possiamo utilizzare i metodi della classe `Matcher` per eseguire le nostre operazioni, come ad esempio `find()` per trovare la prima occorrenza corrispondente, `matches()` per verificare se l'intera stringa corrisponde alla regex, o `replaceAll()` per sostituire tutte le corrispondenze con un altro testo. Ad esempio:

```
String replaced = matcher.replaceAll("regex");
```

Nel nostro esempio, sostituiamo tutte le sequenze di caratteri minuscoli con la parola "regex". 

Ecco un esempio completo di codice:

```
Pattern regex = Pattern.compile("[a-z]+");
Matcher matcher = regex.matcher("Questo è il mio testo di prova");
String replaced = matcher.replaceAll("regex");
System.out.println(replaced);
```

In questo caso, la stringa "Questo è il mio testo di prova" diventerà "regex regex regex regex". 

## Approfondimento sulle espressioni regolari

Le espressioni regolari sono un linguaggio potente per manipolare le stringhe, ma richiedono un po' di tempo e pratica per diventare esperti. Una delle caratteristiche più potenti delle espressioni regolari è l'utilizzo dei gruppi, che ci permette di estrarre in modo selettivo parti di una stringa corrispondente alla nostra regex.

Ad esempio, se avessimo una stringa contenente una data nel formato "dd/mm/yyyy" e volessimo estrarre singolarmente il giorno, il mese e l'anno, potremmo utilizzare gruppi nella nostra regex come segue:

```
Pattern regex = Pattern.compile("([0-9]{2})/([0-9]{2})/([0-9]{4})");
Matcher matcher = regex.matcher("Oggi è il 28/05/2021");
if (matcher.find()) {
    System.out.println("Giorno: " + matcher.group(1));
    System.out.println("Mese: " + matcher.group(2));
    System.out.println("Anno: " + matcher.group(3));
}
```

Questo ci restituirebbe:

```
Giorno: 28
Mese: 05
Anno: 2021
```

Ci sono molte altre funzionalità e caratteristiche avanzate delle espressioni regolari, come le classi di caratteri e le quantificazioni, che possono essere utilizzate per creare regex più complesse e specifiche. Consiglio di esplorare ulteriormente gli approfondimenti suggeriti nella sezione "Vedi anche" per migliorare la propria conoscenza delle espressioni regolari.

## Vedi anche

- [Java Regex Tutorial](https://www.baeldung.com/java-regex)
- [The Java Regex API](https://docs.oracle.com/javase/8/docs/api/java/util/regex/package-summary.html)
- [Regex Tester](https://regex101.com/)