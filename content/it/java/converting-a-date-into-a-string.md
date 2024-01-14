---
title:    "Java: Convertire una data in una stringa"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Perché
La conversione di una data in una stringa è un'operazione comune nella programmazione Java. Questo può essere necessario quando si vuole mostrare una data in un formato diverso o quando si vuole utilizzare la data in una query di database. In questo articolo, vedremo come convertire una data in una stringa utilizzando Java.

## Come fare
Per prima cosa, è necessario creare un oggetto di tipo `Date` utilizzando il costruttore di base. Successivamente, si può utilizzare la classe `SimpleDateFormat` per specificare il formato desiderato della stringa di output. Infine, si può utilizzare il metodo `format` della classe `SimpleDateFormat` per convertire la data in una stringa.

```Java
Date data = new Date();
SimpleDateFormat formatoData = new SimpleDateFormat("dd/MM/yyyy");
String output = formatoData.format(data);
System.out.println(output);
```
Nell'esempio sopra, viene creata una data corrente e viene specificato il formato "dd/MM/yyyy" per la stringa di output. La data viene quindi convertita in una stringa utilizzando il metodo `format` e viene stampata a schermo.

È importante notare che il formato utilizzato deve essere compatibile con il tipo di dato `Date`. Ad esempio, se si cerca di utilizzare un formato che include il giorno della settimana (come "EEE"), si otterrà un'eccezione.

## Approfondimento
La classe `SimpleDateFormat` offre diversi pattern per specificare il formato della data. Alcuni dei più comuni sono:

- `dd` - giorno del mese (01-31)
- `MM` - mese (01-12)
- `yyyy` - anno (ad esempio 2021)
- `EEE` - abbreviazione del giorno della settimana (ad esempio lun per lunedì)
- `MMMM` - nome completo del mese (ad esempio gennaio)
- `HH` - ora in formato 24 ore (00-23)
- `mm` - minuti (00-59)
- `ss` - secondi (00-59)

Per ulteriori informazioni sui pattern disponibili, si consiglia di consultare la documentazione ufficiale di Java.

## Vedi anche
- [Documentazione ufficiale di Java: SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java.util.Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java.util.Calendar](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)