---
title:                "Java: Convertire una stringa in minuscolo"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché
Perché è importante sapere come convertire una stringa in minuscolo? Ci sono molteplici motivi per cui questa è una funzionalità fondamentale da imparare nella programmazione in Java. Ad esempio, lavorare con input utente o fare confronti tra stringhe richiede spesso di essere in grado di gestire correttamente il case delle parole.

## Come Fare
In Java, convertire una stringa in minuscolo è un'operazione molto semplice grazie al metodo `toLowerCase()`. Ecco un esempio di codice che mostra come utilizzarlo:

```Java
String stringa = "Benvenuti in Italia!";
String stringaMinuscola = stringa.toLowerCase();

System.out.println(stringaMinuscola);
```

Questo codice stamperà "benvenuti in italia!" poiché il metodo `toLowerCase()` ha convertito tutte le lettere in minuscolo.

## Approfondimento
È importante notare che il metodo `toLowerCase()` utilizza il sistema di codifica predefinito del sistema operativo per convertire le lettere in minuscolo. Ciò significa che il risultato potrebbe essere diverso su sistemi operativi diversi. Inoltre, questo metodo non solo funziona con le lettere dell'alfabeto latino, ma anche con caratteri di altre lingue.

Inoltre, è possibile utilizzare il metodo `toLowerCase(Locale locale)` per specificare manualmente la lingua di cui si vuole convertire in minuscolo. Ad esempio:

```Java
String stringa = "Benvenuti in Giappone!";
String stringaMinuscola = stringa.toLowerCase(Locale.JAPAN);

System.out.println(stringaMinuscola);
```

Questo codice stamperà "benvenuti in giappone!" poiché abbiamo specificato la lingua giapponese tramite la costante `Locale.JAPAN`.

## Vedi Anche
- Documentazione ufficiale di Java sul metodo `toLowerCase()`: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--
- Tutorial su come lavorare con le stringhe in Java: http://www.html.it/pag/30732/lavorare-con-le-stringhe-in-java/
- Esempi pratici di utilizzo del metodo `toLowerCase()`: https://www.journaldev.com/600/java-string-tolowercase-touppercase