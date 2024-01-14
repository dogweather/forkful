---
title:                "Arduino: Concatenazione di stringhe."
simple_title:         "Concatenazione di stringhe."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché
Spesso nella programmazione di Arduino, è necessario combinare più stringhe di testo per creare un'unica stringa più lunga. Questo può essere fatto tramite una funzione chiamata "concatenazione di stringhe". In questo post, vedremo come utilizzare la concatenazione di stringhe in Arduino e perché può essere utile.

## Come Fare
La concatenazione di stringhe in Arduino è molto semplice. Utilizzando la funzione `concat()` è possibile unire due o più stringhe in una sola. Ecco un esempio di codice:

```Arduino
String primaStringa = "Ciao";
String secondaStringa = "amici";
String terzaStringa = primaStringa.concat(secondaStringa);
```

Nell'esempio sopra, abbiamo creato tre stringhe: "Ciao", "amici" e una terza stringa che unisce le prime due. Il risultato sarà "Ciaoamici". È anche possibile concatenare più di due stringhe utilizzando la funzione `concat()` più volte, come mostrato nell'esempio seguente:

```Arduino
String primaStringa = "Benvenuto";
String secondaStringa = "nel";
String terzaStringa = "mondo di Arduino";
String quartaStringa = primaStringa.concat(secondaStringa).concat(terzaStringa);
```

Il risultato di questo esempio sarà "Benvenutonelmondo di Arduino". Come puoi vedere, la funzione `concat()` può essere utilizzata per unire quante stringhe desideri.

## Approfondimento
La concatenazione di stringhe può essere utile in diverse situazioni. Ad esempio, se stai lavorando con un display LCD, potresti voler visualizzare più messaggi su una sola riga. Utilizzando la concatenazione di stringhe, puoi unire diversi messaggi in un'unica riga e visualizzarli sul display.

Inoltre, la concatenazione di stringhe può essere utilizzata per creare messaggi personalizzati o per unire vari dati o variabili in una sola stringa.

È importante notare che la funzione `concat()` può essere utilizzata solo con le stringhe di tipo `String` nell'Arduino. Se stai lavorando con variabili di tipo `char`, puoi utilizzare la funzione `strcat()` per concatenarle.

## Vedi Anche
- [Funzione `concat()`](https://www.arduino.cc/reference/it/language/functions/strings/concat/)
- [Funzione `strcat()`](https://www.arduino.cc/reference/it/language/functions/communication/serial/strcat/)