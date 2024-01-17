---
title:                "Scaricare una pagina web"
html_title:           "Arduino: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Cosa & Perché?

Scaricare una pagina web significa ottenere il codice sorgente di una pagina web dal server e visualizzarlo sul tuo dispositivo. I programmatori lo fanno per accedere a informazioni importanti, come dati, immagini o testo, e utilizzarle all'interno dei loro programmi.

Come fare:

```
Arduino Client: Download a Web Page
```

Vuoi scaricare una pagina web utilizzando la tua scheda Arduino? E' possibile farlo utilizzando la libreria di Arduino Client. Ecco come:

1. Includi la libreria nel tuo sketch:
```
# include <SPI.h>
# include <Ethernet.h>
```

2. Configura la connessione Internet:
```
byte mac [] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED }; // Cambialo con il tuo MAC
IPAddress ip (192,168,1,100); // Imposta il tuo indirizzo IP
Ethernet.begin (mac, ip); // Inizializza la libreria Ethernet
```

3. Prepara la richiesta HTTP:
```
client.println ("GET /index.html HTTP / 1.1");
client.println ("Host: www.example.com");
```

4. Invia la richiesta e leggi la risposta:
```
client.available ();
while (client.available()) {
 char c = client.read ();
 Serial.print (c); // Visualizza la risposta sulla seriale
}
```

Deep Dive:

In passato, il download di una pagina web richiedeva la necessità di un computer o un dispositivo più avanzato, ma grazie alla tecnologia moderna e alla libreria di Arduino Client, ora puoi farlo utilizzando solo la tua scheda Arduino.

Alcune alternative alla libreria Arduino Client includono la libreria Webduino e l'utilizzo di un modulo Ethernet o WiFi per la connessione Internet. Tuttavia, la libreria Arduino Client è semplice e facile da utilizzare, rendendola la scelta più comune tra i programmatori.

Vale la pena notare che il download di una pagina web utilizzando la libreria Arduino Client richiede la conoscenza di HTTP. Se vuoi approfondire o personalizzare ulteriormente il tuo codice, consulta la documentazione ufficiale di HTTP.

Vedi anche:

- Documentazione ufficiale di Arduino Client: https://www.arduino.cc/en/Reference/Client
- Libreria Webduino: https://github.com/sirleech/Webduino
- Moduli Ethernet e WiFi per Arduino: https://www.arduino.cc/en/Guide/ArduinoEthernetShield
- Documentazione ufficiale di HTTP: https://tools.ietf.org/html/rfc1945