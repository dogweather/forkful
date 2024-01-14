---
title:                "Java: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

La conversione di una data in una stringa è un'attività comune nel mondo della programmazione, specialmente quando si lavora con dati temporali. Sapere come farlo è essenziale per aiutare a rappresentare accuratamente le informazioni e facilitare la loro manipolazione.

## Come fare

Per convertire una data in una stringa in Java, è necessario utilizzare il metodo `format()` della classe `SimpleDateFormat`. Ecco un esempio di codice per convertire una data nel formato "dd/MM/yyyy hh:mm:ss":

```
SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy hh:mm:ss");
Date date = new Date(); //data corrente
String dataStringa = sdf.format(date);

System.out.println(dataStringa); //stamperà "21/06/2021 14:30:20"
```

Ecco una lista dei principali formati di data che è possibile utilizzare con `SimpleDateFormat`:

- `dd` – giorno del mese (01-31)
- `MM` – mese in formato numerico (01-12)
- `MMM` – mese abbreviato (gen, feb, mar, ecc.)
- `MMMM` - mese completo (gennaio, febbraio, marzo, ecc.)
- `yyyy` – anno a 4 cifre
- `yy` – anno a 2 cifre
- `HH` – ora del giorno nel formato 24 ore (00-23)
- `hh` – ora del giorno nel formato 12 ore (01-12)
- `mm` – minuti (00-59)
- `ss` – secondi (00-59)

## Approfondimento

Oltre ad utilizzare i formati di data forniti dalla classe `SimpleDateFormat`, è possibile anche personalizzare il formato in base alle proprie esigenze utilizzando dei caratteri speciali. Per esempio, `E` rappresenta il giorno della settimana (lun, mar, mer, ecc.), `a` rappresenta AM o PM, `z` rappresenta il fuso orario e così via. È possibile trovare una lista completa di questi caratteri nella documentazione ufficiale di Java.

Inoltre, è importante considerare anche il formato della data nel codice sorgente. Se si vuole utilizzare un formato diverso da quello di default della JVM, è necessario impostarlo utilizzando il metodo `setDefault()` prima di convertire la data in stringa.

## Vedi anche

- [Documentazione ufficiale di Java](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Tutorial su come convertire una data in una stringa in Java](https://www.baeldung.com/java-string-to-date-conversion)
- [Altri esempi di utilizzo di `SimpleDateFormat`](https://www.techiedelight.com/formatting-date-in-java-simpledateformat/)