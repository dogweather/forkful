---
title:                "Java: Maiuscolare una stringa"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalizzare una stringa è una pratica comune nella programmazione per rendere più leggibili e uniformi i dati. Per esempio, una stringa come "ciao a tutti" diventerà "Ciao a tutti" dopo la capitalizzazione.

## Come Fare

Per capitalizzare una stringa in Java, è possibile utilizzare il metodo `toUpperCase()` della classe `String`. Di seguito viene mostrato un esempio di codice che visualizza l'utilizzo di questo metodo:

```Java
String str = "ciao a tutti";
String maiuscolo = str.toUpperCase();
System.out.println(maiuscolo);
```

L'output di questo codice sarà "CIAO A TUTTI".

Si possono anche capitalizzare solo il primo carattere di una stringa utilizzando il metodo `substring()` insieme a `toUpperCase()`. Di seguito viene mostrato un esempio di codice:

```Java
String str = "ciao a tutti";
String primo = str.substring(0, 1).toUpperCase();
String rimanente = str.substring(1);
String maiuscolo = primo + rimanente;
System.out.println(maiuscolo);
```

L'output di questo codice sarà "Ciao a tutti".

## Approfondimento

La capitalizzazione delle stringhe in Java segue le regole dell'alfabeto della lingua inglese. Ciò significa che le lettere maiuscole sono diverse dalle lettere minuscole e che le lettere accentate vengono convertite in maiuscolo con il metodo `toUpperCase()`. È importante notare che il metodo `toUpperCase()` crea una nuova stringa mentre il metodo `toUpperCase()` modifica direttamente la stringa a cui viene applicato.

## Vedi Anche

- Documentazione ufficiale del metodo `toUpperCase()`: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--
- Tutorial di Java sulla manipolazione delle stringhe: https://www.w3schools.com/java/java_strings.asp