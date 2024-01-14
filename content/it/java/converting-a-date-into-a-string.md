---
title:    "Java: Convertire una data in una stringa"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Perché

 Ci sono molte situazioni in cui è necessario convertire una data in una stringa in Java. Ad esempio, in un programma di gestione delle prenotazioni, potresti dover mostrare la data di prenotazione come stringa per la stampa. O forse hai bisogno di salvare la data come stringa in un database o in un file di testo. Quando ciò accade, è importante sapere come convertire correttamente una data in una stringa per evitare errori o problemi di formattazione.

##Come fare

Per convertire una data in una stringa, è necessario utilizzare il metodo `format()` della classe `java.text.SimpleDateFormat`. Ecco un esempio di codice:

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateToString {
  public static void main(String[] args) {
    // creare una nuova istanza della classe SimpleDateFormat
    SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
    
    // ottenere la data corrente
    Date currentDate = new Date();
    
    // utilizzare il metodo format per convertire la data in una stringa
    String stringDate = dateFormat.format(currentDate);
    
    // stampare la data come stringa
    System.out.println("Data corrente: " + stringDate);
  }
}
```

Ecco l'output di questo esempio:

```
Data corrente: 21/11/2021
```

Come puoi vedere, la data è stata convertita in una stringa con il formato specificato nella classe `SimpleDateFormat`.

Puoi anche specificare un altro formato, ad esempio `"yyyy-MM-dd"`, per ottenere la data in un formato diverso. Inoltre, è possibile utilizzare metodi come `parse()` per convertire una stringa in una data.

## Approfondimento

La classe `SimpleDateFormat` ha molti metodi utili che consentono di gestire la conversione tra date e stringhe in modo più preciso. Ad esempio, puoi utilizzare `setLenient(false)` per specificare che il parsing delle date è rigoroso, in modo che le date errate non vengano convertite in modo errato senza alcun errore.

Inoltre, è importante fare attenzione al formato della data specificato nella classe `SimpleDateFormat`. Se non corrisponde al formato della data, potresti ottenere una `ParseException` durante la conversione.

## Vedi anche

- [Documentazione ufficiale di SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Tutorial su come convertire una data in una stringa in Java](https://www.baeldung.com/java-date-to-string)
- [Articolo su come formattare le date in Java](https://www.oreilly.com/library/view/java-cookbook-3rd/9780596001707/ch12s09.html)