---
title:                "Scrivere su standard error"
html_title:           "Arduino: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error può essere utile per diagnosticare e risolvere eventuali errori all'interno del codice. È uno strumento prezioso per gli sviluppatori in fase di debug.

## Come fare

Per scrivere su standard error in Arduino, puoi utilizzare la funzione `Serial.println()` seguita dal messaggio che desideri visualizzare. Ad esempio:

```Arduino
Serial.println("Errore: il valore inserito è troppo alto.");
```

Questa istruzione scriverà il messaggio "Errore: il valore inserito è troppo alto" sulla porta di comunicazione seriale, che può essere visualizzato sulla console del computer tramite un'interfaccia seriale.

Puoi anche utilizzare la funzione `Serial.print()` per scrivere solo una parte del messaggio, seguita dalla funzione `Serial.println()` per andare a capo. Ad esempio:

```Arduino
Serial.print("Valore inserito non valido. Riprova.");
Serial.println("."); //andando a capo
```

Questo scriverà il messaggio "Valore inserito non valido. Riprova." sulla stessa linea della console, seguito da un andando a capo che inserirà il cursore su una nuova linea.

## Approfondimento

La funzione `Serial.println()` in realtà invia il messaggio alla porta di comunicazione seriale come un'array di caratteri (char array), quindi è importante conoscere il tipo di dato che si sta utilizzando. Se si utilizza una variabile di un altro tipo (ad esempio un int o un float), sarà necessario convertirla in una stringa utilizzando la funzione `String()`. Ad esempio:

```Arduino
int errore = 404;
String messaggio = String("Codice di errore: ") + String(errore);
Serial.println(messaggio);
```

Questo scriverà il messaggio "Codice di errore: 404".

È inoltre possibile utilizzare la funzione `Serial.write()` per scrivere singoli byte sulla porta seriale. Questo è utile se si deve inviare un valore numerico compreso tra 0 e 255, come ad esempio un codice ASCII o un segnale PWM. Ad esempio:

```Arduino
byte codice = 65; //equivalente al carattere 'A' nella tabella ASCII
Serial.write(codice);
```

Questo scriverà il carattere 'A' sulla porta seriale.

## Vedi anche

- Documentazione Arduino su scrittura su standard error: https://www.arduino.cc/en/Serial/Print
- Tutorial di Arduino sulle stringhe: https://www.arduino.cc/en/Reference/String