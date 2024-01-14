---
title:    "Java: Cancellare caratteri corrispondenti a uno schema"
keywords: ["Java"]
---

{{< edit_this_page >}}

## "Perché"

Nella programmazione Java, è spesso necessario manipolare stringhe di testo per ottenere un determinato risultato. Talvolta, ci si trova di fronte alla necessità di eliminare caratteri che corrispondono a un certo modello o pattern. Ma perché dovremmo fare ciò? Ci possono essere diverse ragioni che spingono a effettuare questa operazione, come ad esempio la pulizia dei dati o la formattazione di un testo in un formato specifico. In questo articolo, esploreremo come eliminare caratteri corrispondenti a un pattern in Java.

## "Come fare"

Per iniziare, vediamo come è possibile eliminare i caratteri corrispondenti a un certo pattern in Java. Di seguito è riportato un esempio di codice che mostra come farlo utilizzando il metodo `replaceAll()` della classe String:

```Java
String input = "Questo è un esempio di testo con caratteri indesiderati/k#29";
String output = input.replaceAll("[^a-zA-Z0-9 ]", ""); 
System.out.println(output); // Output: "Questo è un esempio di testo con caratteri indesideratik29"
```

Nell'esempio sopra, stiamo utilizzando un'espressione regolare per eliminare tutti i caratteri non alfanumerici e non spazi dalla stringa di input. Il metodo `replaceAll()` sostituisce ogni occorrenza del pattern specificato con la stringa vuota, ottenendo così una stringa senza i caratteri indesiderati. È possibile modificare l'espressione regolare in base alle proprie esigenze per eliminare caratteri diversi.

Un'altra opzione è utilizzare il metodo `replace()` per sostituire un singolo carattere con un altro. Ad esempio, se si desidera semplicemente eliminare tutti i caratteri di punteggiatura da una stringa, si può fare così:

```Java
String input = "Questo è un testo con della punteggiatura!@#$%&";
String output = input.replace(".", "").replace("!", "").replace("#", "");
System.out.println(output); // Output: "Questo è un testo con della punteggiatura"
```

Come si può vedere, il metodo `replace()` può essere usato più volte per sostituire diversi caratteri con la stringa vuota.

## "Approfondimento"

Ora che sappiamo come eliminare i caratteri corrispondenti a un pattern in Java, possiamo approfondire l'argomento parlando di espressioni regolari. Un'espressione regolare è una sequenza di caratteri che descrive un modello di ricerca e sostituzione su una stringa. Viene comunemente utilizzata per la manipolazione di testo in diverse piattaforme di programmazione.

In Java, le espressioni regolari sono supportate dalla classe `java.util.regex.Pattern` e possono essere utilizzate tramite i metodi `matches()` e `replaceAll()` della classe String. Ci sono diverse funzioni e simboli che possono essere utilizzati nelle espressioni regolari, come ad esempio:

- `[ ]` per definire un carattere o un insieme di caratteri
- `[^ ]` per escludere un carattere o un insieme di caratteri
- `*` per indicare che il carattere precedente può essere ripetuto 0 o più volte
- `+` per indicare che il carattere precedente deve essere ripetuto 1 o più volte
- `?` per indicare che il carattere precedente può essere ripetuto 0 o 1 volta
- `\d` per indicare un carattere numerico
- `\w` per indicare un carattere alfanumerico

Esempi di espressioni regolari possono essere trovati in grande quantità su internet e ci sono anche tool online che permettono di testarle e sperimentare con esse.

## "Vedi anche"

- [Java String replaceAll() method documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-)
- [Java String replace() method documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replace-char-char-)
- [Java Regular Expression Tutorial](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)
- [Online