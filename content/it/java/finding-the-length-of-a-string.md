---
title:                "Trova la lunghezza di una stringa"
html_title:           "Java: Trova la lunghezza di una stringa"
simple_title:         "Trova la lunghezza di una stringa"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Perché dovresti essere interessato a trovare la lunghezza di una stringa in Java? Beh, sapere la lunghezza di una stringa è utile per molte ragioni. Ad esempio, potresti voler verificare se una stringa è troppo lunga per un determinato campo di input, o semplicemente per manipolare correttamente i dati in base alla loro lunghezza.

## Come Fare

Per trovare la lunghezza di una stringa in Java, è possibile utilizzare il metodo `length()` della classe String. Iniziamo con un esempio di codice:

```Java
String stringa = "Ciao, mondo!";
int lunghezza = stringa.length();
System.out.println(lunghezza);
```

Questo codice dovrebbe produrre in output il numero 13, poiché la stringa "Ciao, mondo!" contiene 13 caratteri. Ma cosa succede se vogliamo contare solo i caratteri alfabetici anziché tutti i caratteri? In tal caso, possiamo utilizzare il metodo `replaceAll()` per eliminare tutti i caratteri non alfabetici prima di chiamare il metodo `length()`:

```Java
String stringa = "Ciao, mondo!";
stringa = stringa.replaceAll("[^a-zA-Z]", ""); // rimuove tutti i caratteri non alfabetici
int lunghezza = stringa.length();
System.out.println(lunghezza);
```

In questo caso, il numero di caratteri in output sarebbe 9 anziché 13, in quanto abbiamo rimosso le virgole e lo spazio vuoto.

## Approfondimento

Ora che abbiamo visto come trovare la lunghezza di una stringa, è utile sapere come funziona il metodo `length()` sotto il cofano. In realtà, questo metodo è una variabile di istanza privata della classe String e viene inizializzato durante la creazione della stringa. In altre parole, ogni volta che creiamo una stringa, viene anche creata una variabile di istanza privata che tiene traccia della sua lunghezza. Quando chiamiamo il metodo `length()`, semplicemente restituiamo il valore di questa variabile, il che rende molto efficiente il calcolo della lunghezza di una stringa.

## Vedi Anche

- Metodo `replaceAll()` della classe String in Java: [Ilink](https://www.java.com/it/download/)
- Documentazione ufficiale del metodo `length()` della classe String: [Ilink](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#length())