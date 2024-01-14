---
title:    "Arduino: Scrivere su standard error"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error è un importante strumento per il debugging e il monitoraggio dei programmi su Arduino. È utile per visualizzare messaggi di errore e informazioni di sistema durante lo sviluppo del codice.

## Come fare

Per scrivere su standard error su Arduino, è necessario utilizzare il comando `Serial.print()` seguito dal messaggio o dalla variabile che si desidera stampare. Ad esempio:

```
Arduino Serial.print("Errore: Impossibile connettersi al modulo WiFi");
```

Questo stamperà il messaggio "Errore: Impossibile connettersi al modulo WiFi" su standard error. È anche possibile utilizzare il comando `Serial.println()` per stampare una nuova riga alla fine del messaggio.

```
Serial.println("Messaggio importante");
```

Questo stamperà il messaggio "Messaggio importante" su una nuova riga in standard error.

Per ricevere l'output su un monitor seriale o su un terminale, è necessario utilizzare un adattatore USB-seriale o un modulo seriale Bluetooth.

## Approfondimento

L'utilizzo di standard error è particolarmente utile quando si eseguono operazioni di debugging. Mentre `Serial.print()` stampa le informazioni sul monitor seriale, `Serial.println()` aggiunge una nuova riga ogni volta che viene chiamato, rendendo più facile la lettura dei messaggi. È anche possibile utilizzare il comando `Serial.write()` per inviare dati binari a standard error.

Inoltre, è possibile collegare un display LCD al tuo Arduino e utilizzarlo come un monitor parallelo per stampare gli output di standard error. Ciò consente di monitorare il programma in tempo reale senza dover collegarlo a un computer.

## Vedi anche

- [Guida all'uso di standard error su Arduino] (link)
- [Serial.print() documentazione ufficiale] (link)
- [Moduli seriale per Arduino] (link)