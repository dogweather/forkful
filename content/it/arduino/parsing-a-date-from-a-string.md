---
title:                "Analisi di una data da una stringa"
html_title:           "Arduino: Analisi di una data da una stringa"
simple_title:         "Analisi di una data da una stringa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa e perche?
L'estrazione di una data da una stringa è il processo di ottenere una data in un formato specifico - come giorno, mese e anno - da una stringa di testo più lunga. I programmatori spesso eseguono questa operazione quando devono manipolare o utilizzare date in un progetto o programma.

## Come fare:
Nell'Arduino, esistono diverse librerie e funzioni che aiutano nella conversione di una stringa in una data formattata. Ecco un esempio di codice che utilizza la libreria Time e la funzione `strptime` per estrarre una data dalla stringa "20/09/2021" e stamparla nel formato "gg mm aaaa":

```Arduino
#include <Time.h>
...
String data = "20/09/2021";
...
tmElements_t data_format = {0};
strptime(data.c_str(), "%d/%m/%Y", &data_format);
...
Serial.print(data_format.Day);
Serial.print(" ");
Serial.print(data_format.Month);
Serial.print(" ");
Serial.print(data_format.Year);
```
Output:
`20 09 2021`

## Approfondimento:
Estrarre una data da una stringa è stato un problema comune per i programmatori fin dai primi giorni della programmazione. In passato, era spesso necessario scrivere una funzione personalizzata per l'estrazione della data in base al formato specifico della stringa. Tuttavia, grazie alle librerie e alle funzioni disponibili nei moderni ambienti di sviluppo come Arduino, ora possiamo farlo in modo più efficiente.

## Vedi anche:
- [Documentazione di Arduino sulla libreria Time](https://www.arduino.cc/en/Reference/time)
- [Esempio di utilizzo della funzione `strptime`](https://www.geeksforgeeks.org/strftime-function-in-c/)
- [Altro esempio di parsing di date con Arduino](https://www.hackster.io/frankjnu/diy-clock-using-arduino-and-ds3231-rtc-module-b3fe4a)