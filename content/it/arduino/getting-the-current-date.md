---
title:                "Ottenere la data corrente"
html_title:           "Arduino: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Ottenere la data corrente è un'operazione comune nei programmi, che consiste nel recuperare la data corrente dal sistema in cui viene eseguito. I programmatori spesso utilizzano questa informazione per poter tenere traccia del tempo passato, programmare eventi futuri o semplicemente visualizzare la data sul display.

## Come fare:

Utilizzare la funzione `millis()` è un modo semplice per ottenere la data corrente in millisecondi. Questa funzione restituisce il numero di millisecondi trascorsi da quando il programma è stato avviato. Ad esempio, il codice seguente visualizza la data corrente sul monitor seriale:

```
Arduino ... Serial.begin(9600);

Arduino ... Serial.print("La data corrente è: ");
Arduino ... Serial.println(millis() / 1000); // millis() restituisce i millisecondi totali, quindi dividiamo per 1000 per ottenere i secondi
```

L'output sarà qualcosa del tipo "La data corrente è: 82851".

## Approfondimento:

Alternativamente, è possibile utilizzare un modulo RTC (Real Time Clock) che si collega direttamente alla scheda Arduino e fornisce un'accurata data e ora. Un esempio è il modulo DS1307 RTC. In questo caso, è necessario utilizzare una specifica libreria che consente di comunicare con il modulo e leggere la data corrente.

Un'altra opzione è quella di utilizzare una fonte esterna, come un server NTP (Network Time Protocol) per ottenere la data e ora correnti tramite una connessione internet. Ciò può risultare utile se si vuole sincronizzare il dispositivo con una fonte affidabile e precisa.

## Vedi anche:

- [Funzione millis()](https://www.arduino.cc/reference/en/language/functions/time/millis/)
- [Modulo DS1307 RTC](https://www.adafruit.com/product/264)
- [Libreria per DS1307 RTC](https://github.com/adafruit/RTClib)
- [Server NTP](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/ntptime.html)