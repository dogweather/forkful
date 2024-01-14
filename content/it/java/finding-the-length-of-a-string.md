---
title:    "Java: Trova la lunghezza di una stringa"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza di una stringa è un'attività comune nell'ambito della programmazione, soprattutto quando si lavora con dati di input forniti dagli utenti o da file esterni. Sapere quanto è lunga una stringa può essere utile per validare l'input o per manipolare i dati in maniera efficace.

## Come fare

Per trovare la lunghezza di una stringa in Java, utilizzeremo il metodo `length()` della classe `String`. Questo metodo restituisce un intero che rappresenta il numero di caratteri nella stringa. Vediamo un esempio pratico:

```Java
String nome = "Mia"; //dichiariamo una stringa
int lunghezza = nome.length(); //chiamiamo il metodo length() sulla stringa
System.out.println(lunghezza); //stampa "3" poiché ci sono 3 caratteri nella parola "Mia"
```

L'output di questo codice sarà "3", poiché ci sono appunto 3 caratteri nella parola "Mia". Possiamo anche utilizzare questo metodo per controllare la lunghezza di una stringa fornita dall'utente:

```Java
Scanner sc = new Scanner(System.in); //creiamo uno scanner per leggere l'input
System.out.println("Inserisci una parola: ");
String parola = sc.nextLine(); //leggiamo l'input dell'utente
int lunghezza = parola.length(); //chiamiamo il metodo length() sulla stringa inserita
System.out.println("Questa parola ha " + lunghezza + " caratteri.");
```

Se l'utente inserisce ad esempio la parola "casa", l'output sarà "Questa parola ha 4 caratteri.".

## Approfondimento

Quando utilizziamo il metodo `length()` su una stringa vuota, ovvero una stringa senza alcun carattere, il risultato sarà sempre 0. Inoltre, questo metodo non conta solo i caratteri alfanumerici, ma anche gli spazi e i caratteri speciali come ad esempio le virgole o i punti esclamativi.

In Java, possiamo trovare la lunghezza di qualsiasi oggetto che implementi l'interfaccia `CharSequence`, che include anche la classe `StringBuilder` e `StringBuffer`.

## Vedi anche

* [Documentazione ufficiale di Java sul metodo `length()`](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#length())
* [Tutorial su come utilizzare il metodo `length()`](https://www.geeksforgeeks.org/string-length-method-in-java/)