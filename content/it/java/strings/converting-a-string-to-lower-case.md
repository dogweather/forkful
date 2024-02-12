---
title:                "Conversione di una stringa in minuscolo"
aliases:
- /it/java/converting-a-string-to-lower-case/
date:                  2024-01-20T17:38:28.833887-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversione di una stringa in minuscolo"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Convertire una stringa in minuscolo significa trasformare tutti i caratteri al suo interno in lettere minuscole. I programmatori lo fanno per uniformare i dati, facilitare confronti insensibili alle maiuscole e migliorare la consistenza dell'input utente.

## How to:
Java usa il metodo `toLowerCase()` per convertire una stringa in minuscolo. Vediamo un esempio pratico:

```java
public class StringToLowercase {
    public static void main(String[] args) {
        String original = "Ciao Mondo!";
        String lowercased = original.toLowerCase();
        System.out.println(lowercased);
    }
}
```
Output:
```
ciao mondo!
```

## Deep Dive
Java gestisce la conversione in minuscolo da Java 1.0, aiutando i programmatori a standardizzare le stringhe. Altre linghe possono avere funzioni simili, come `lower()` in Python.

In Java, `toLowerCase()` può usare regole locali (Locale) per gestire casi speciali legati alla lingua. Ad esempio, `toUpperCase()` in turco converte 'i' minuscola senza punto in 'İ' maiuscola con punto.

Ecco un esempio con Locale:

```java
String original = "Fenomeno";
String turkishLowercased = original.toLowerCase(new Locale("tr", "TR"));
System.out.println(turkishLowercased); // "fenomeno" con 'i' senza punto
```

Alternativamente, `toLowerCase(Locale.ROOT)` è neutrale e ignora le particolarità locali.

## See Also
Consulta la documentazione ufficiale di Java per `String.toLowerCase()` [qui](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--).

Per un'immersione più profonda nelle peculiarità di Locale, visita il link seguente: [Locale specific behaviors](https://docs.oracle.com/javase/tutorial/i18n/locale/index.html). 

Per la gestione e la manipolazione delle stringhe in altre lingue, Python offre una prospettiva interessante, il cui dettaglio lo trovi [qui](https://docs.python.org/3/library/stdtypes.html#str.lower).
